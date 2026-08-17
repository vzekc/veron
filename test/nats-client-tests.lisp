;;; -*- Mode: Lisp -*-

;;; Tests for the VERON.NATS client — frame encoding and protocol flow
;;; against a mock TCP server.

(in-package #:veron-tests)

;;; --- Frame encoding (no socket)

(define-test nats-pub-frame-bytes ()
  (let* ((frame (veron.nats::encode-pub-frame "veron.session.login" nil "hi"))
         (expected (concatenate 'string
                                "PUB veron.session.login 2"
                                (string #\Return) (string #\Linefeed)
                                "hi"
                                (string #\Return) (string #\Linefeed))))
    (assert (equalp frame (babel:string-to-octets expected :encoding :utf-8)))))

(define-test nats-pub-frame-with-reply ()
  (let ((frame (veron.nats::encode-pub-frame "a.b" "reply.sub" "x")))
    (assert (search "PUB a.b reply.sub 1"
                    (babel:octets-to-string frame :encoding :utf-8)))))

(define-test nats-pub-frame-utf8-payload ()
  (let* ((payload "hällo")
         (frame (veron.nats::encode-pub-frame "t" nil payload))
         (utf8 (babel:string-to-octets payload :encoding :utf-8))
         (expected-header (format nil "PUB t ~D~C~C" (length utf8)
                                  #\Return #\Linefeed)))
    (assert (alexandria:starts-with-subseq
             (babel:string-to-octets expected-header :encoding :ascii)
             frame))
    (assert (= (length frame) (+ (length expected-header) (length utf8) 2)))))

(define-test nats-sub-frame-bytes ()
  (let ((frame (veron.nats::encode-sub-frame "veron.>" nil 42)))
    (assert (equalp frame
                    (babel:string-to-octets
                     (format nil "SUB veron.> 42~C~C" #\Return #\Linefeed)
                     :encoding :ascii)))))

(define-test nats-sub-frame-with-queue ()
  (let ((frame (veron.nats::encode-sub-frame "a.b" "workers" 7)))
    (assert (search "SUB a.b workers 7"
                    (babel:octets-to-string frame :encoding :ascii)))))

(define-test nats-unsub-frame-bytes ()
  (let ((f1 (veron.nats::encode-unsub-frame 3 nil))
        (f2 (veron.nats::encode-unsub-frame 3 10)))
    (assert (equalp f1 (babel:string-to-octets
                        (format nil "UNSUB 3~C~C" #\Return #\Linefeed)
                        :encoding :ascii)))
    (assert (equalp f2 (babel:string-to-octets
                        (format nil "UNSUB 3 10~C~C" #\Return #\Linefeed)
                        :encoding :ascii)))))

(define-test nats-pong-frame-bytes ()
  (let ((frame (veron.nats::encode-pong-frame)))
    (assert (equalp frame
                    (babel:string-to-octets
                     (format nil "PONG~C~C" #\Return #\Linefeed)
                     :encoding :ascii)))))

(define-test nats-parse-url-defaults ()
  (multiple-value-bind (host port user pass) (veron.nats::parse-url "nats://")
    (assert (equal host "127.0.0.1"))
    (assert (= port 4222))
    (assert (null user))
    (assert (null pass))))

(define-test nats-parse-url-host-port ()
  (multiple-value-bind (host port) (veron.nats::parse-url "nats://broker.lan:1234")
    (assert (equal host "broker.lan"))
    (assert (= port 1234))))

(define-test nats-parse-url-credentials ()
  (multiple-value-bind (host port user pass tls-p)
      (veron.nats::parse-url "nats://bob:secret@host:9999")
    (assert (equal host "host"))
    (assert (= port 9999))
    (assert (equal user "bob"))
    (assert (equal pass "secret"))
    (assert (null tls-p))))

(define-test nats-parse-url-tls ()
  (multiple-value-bind (host port user pass tls-p)
      (veron.nats::parse-url "tls://veron:hunter2@nats.example.org:4222")
    (assert (equal host "nats.example.org"))
    (assert (= port 4222))
    (assert (equal user "veron"))
    (assert (equal pass "hunter2"))
    (assert (eq tls-p t))))

(define-test nats-parse-url-plain-returns-nil-tls ()
  (multiple-value-bind (host port user pass tls-p)
      (veron.nats::parse-url "nats://host:4222")
    (declare (ignore port user pass))
    (assert (equal host "host"))
    (assert (null tls-p))))

;;; --- Mock NATS server

(defclass mock-nats-server ()
  ((port :initarg :port :reader mock-port)
   (listener :initarg :listener :accessor mock-listener)
   (accept-thread :initform nil :accessor mock-accept-thread)
   (running :initform t :accessor mock-running)
   (received :initform (sb-concurrency:make-mailbox) :reader mock-received)
   (connections :initform nil :accessor mock-connections)
   (lock :initform (bt:make-lock) :reader mock-lock)))

(defun mock-read-crlf-line (stream)
  (let ((buf (make-array 64 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0))
        (last 0))
    (loop for byte = (read-byte stream)
          do (when (and (= last 13) (= byte 10))
               (vector-pop buf)
               (return (map 'string #'code-char buf)))
             (vector-push-extend byte buf)
             (setf last byte))))

(defun mock-connection-handler (server socket)
  (let ((stream (usocket:socket-stream socket)))
    (handler-case
        (progn
          (write-sequence (babel:string-to-octets
                           (format nil "INFO {}~C~C" #\Return #\Linefeed)
                           :encoding :ascii)
                          stream)
          (finish-output stream)
          (loop while (mock-running server) do
            (let ((line (mock-read-crlf-line stream)))
              (cond
                ((zerop (length line)))
                ((alexandria:starts-with-subseq "CONNECT " line)
                 (sb-concurrency:send-message (mock-received server)
                                              (list :connect line)))
                ((alexandria:starts-with-subseq "PUB " line)
                 (let* ((rest (subseq line 4))
                        (parts (loop with start = 0 with len = (length rest)
                                     while (< start len)
                                     for space = (position #\Space rest :start start)
                                     collect (subseq rest start (or space len))
                                     do (setf start (if space (1+ space) len))))
                        (size (parse-integer (car (last parts))))
                        (payload (make-array size :element-type '(unsigned-byte 8))))
                   (read-sequence payload stream)
                   (read-byte stream) (read-byte stream)
                   (sb-concurrency:send-message (mock-received server)
                                                (list :pub line payload))))
                ((alexandria:starts-with-subseq "SUB " line)
                 (sb-concurrency:send-message (mock-received server)
                                              (list :sub line)))
                ((alexandria:starts-with-subseq "UNSUB " line)
                 (sb-concurrency:send-message (mock-received server)
                                              (list :unsub line)))
                ((string= line "PING")
                 (sb-concurrency:send-message (mock-received server) (list :ping)))
                ((string= line "PONG")
                 (sb-concurrency:send-message (mock-received server) (list :pong)))))))
      (error ()))
    (ignore-errors (usocket:socket-close socket))))

(defun mock-accept-loop (server)
  (handler-case
      (loop while (mock-running server) do
        (let ((socket (usocket:socket-accept (mock-listener server))))
          (bt:with-lock-held ((mock-lock server))
            (push socket (mock-connections server)))
          (bt:make-thread (lambda () (mock-connection-handler server socket))
                          :name "mock-nats-conn")))
    (error ())))

(defun start-mock-server ()
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0
                                          :element-type '(unsigned-byte 8)
                                          :reuse-address t))
         (port (usocket:get-local-port listener))
         (server (make-instance 'mock-nats-server :port port :listener listener)))
    (setf (mock-accept-thread server)
          (bt:make-thread (lambda () (mock-accept-loop server))
                          :name "mock-nats-accept"))
    server))

(defun stop-mock-server (server)
  (setf (mock-running server) nil)
  (ignore-errors (usocket:socket-close (mock-listener server)))
  (bt:with-lock-held ((mock-lock server))
    (dolist (c (mock-connections server))
      (ignore-errors (usocket:socket-close c)))
    (setf (mock-connections server) nil))
  (when (and (mock-accept-thread server)
             (bt:thread-alive-p (mock-accept-thread server)))
    (ignore-errors (bt:destroy-thread (mock-accept-thread server)))))

(defun mock-current-connection (server)
  (bt:with-lock-held ((mock-lock server))
    (first (mock-connections server))))

(defun mock-send-bytes (server bytes)
  (let ((conn (mock-current-connection server)))
    (unless conn (error "no client connection to mock server"))
    (let ((stream (usocket:socket-stream conn)))
      (write-sequence bytes stream)
      (finish-output stream))))

(defun mock-close-current (server)
  (let ((conn (mock-current-connection server)))
    (when conn
      (bt:with-lock-held ((mock-lock server))
        (setf (mock-connections server) (remove conn (mock-connections server))))
      ;; Shutdown before close: on Linux, close() while another thread is
      ;; blocked in read() may not send FIN until the read returns. Shutdown
      ;; tears down the TCP connection immediately so the client sees EOF.
      (ignore-errors
       (sb-bsd-sockets:socket-shutdown (usocket:socket conn) :direction :io))
      (ignore-errors (usocket:socket-close conn)))))

(defun wait-for-frame (server &key (timeout 3.0))
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (multiple-value-bind (msg received-p)
          (sb-concurrency:receive-message-no-hang (mock-received server))
        (when received-p (return msg))
        (when (> (get-internal-real-time) deadline) (return nil))
        (sleep 0.01)))))

(defun wait-for-frame-matching (server predicate &key (timeout 3.0))
  "Drain frames until one matches PREDICATE (or timeout)."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (multiple-value-bind (msg received-p)
          (sb-concurrency:receive-message-no-hang (mock-received server))
        (cond ((and received-p (funcall predicate msg)) (return msg))
              ((> (get-internal-real-time) deadline) (return nil))
              (t (sleep 0.01)))))))

(defmacro with-mock-server-and-client ((server-var client-var) &body body)
  `(let ((,server-var (start-mock-server)))
     (unwind-protect
          (let ((,client-var (veron.nats:connect
                              (format nil "nats://127.0.0.1:~D" (mock-port ,server-var)))))
            (declare (ignorable ,client-var))
            (unwind-protect (locally ,@body)
              (ignore-errors (veron.nats:disconnect ,client-var))))
       (stop-mock-server ,server-var))))

;;; --- Protocol flow

(define-test nats-flow-sends-connect ()
  (with-mock-server-and-client (server client)
    (declare (ignore client))
    (let ((frame (wait-for-frame-matching server
                  (lambda (f) (eq (car f) :connect))
                  :timeout 3.0)))
      (assert frame)
      (assert (search "\"name\":\"veron\"" (second frame))))))

(define-test nats-flow-publishes ()
  (with-mock-server-and-client (server client)
    (wait-for-frame-matching server (lambda (f) (eq (car f) :connect)))
    (veron.nats:publish client "veron.session.login" "payload-body")
    (let ((frame (wait-for-frame-matching server (lambda (f) (eq (car f) :pub))
                                          :timeout 3.0)))
      (assert frame)
      (assert (equal "PUB veron.session.login 12" (second frame)))
      (assert (equalp (third frame)
                      (babel:string-to-octets "payload-body" :encoding :utf-8))))))

(define-test nats-flow-subscribe-dispatches-msg ()
  (with-mock-server-and-client (server client)
    (wait-for-frame-matching server (lambda (f) (eq (car f) :connect)))
    (let ((received (sb-concurrency:make-mailbox)))
      (veron.nats:subscribe client "veron.>"
                            (lambda (subject reply payload)
                              (sb-concurrency:send-message received
                                (list subject reply payload))))
      (let ((sub-frame (wait-for-frame-matching server
                        (lambda (f) (eq (car f) :sub)))))
        (assert sub-frame))
      (mock-send-bytes server
                       (babel:string-to-octets
                        (format nil "MSG veron.demo 1 5~C~Chello~C~C"
                                #\Return #\Linefeed #\Return #\Linefeed)
                        :encoding :ascii))
      (let* ((deadline (+ (get-internal-real-time)
                          (* 3.0 internal-time-units-per-second)))
             (got nil))
        (loop until got do
          (multiple-value-bind (msg received-p)
              (sb-concurrency:receive-message-no-hang received)
            (when received-p (setf got msg))
            (when (> (get-internal-real-time) deadline) (return))
            (sleep 0.01)))
        (assert got)
        (assert (equal (first got) "veron.demo"))
        (assert (null (second got)))
        (assert (equalp (third got)
                        (babel:string-to-octets "hello" :encoding :ascii)))))))

(define-test nats-flow-ping-pong ()
  (with-mock-server-and-client (server client)
    (declare (ignore client))
    (wait-for-frame-matching server (lambda (f) (eq (car f) :connect)))
    (mock-send-bytes server
                     (babel:string-to-octets
                      (format nil "PING~C~C" #\Return #\Linefeed)
                      :encoding :ascii))
    (let ((pong (wait-for-frame-matching server (lambda (f) (eq (car f) :pong))
                                         :timeout 3.0)))
      (assert pong))))

(define-test nats-flow-reconnect-resubscribes ()
  (with-mock-server-and-client (server client)
    (wait-for-frame-matching server (lambda (f) (eq (car f) :connect)))
    (veron.nats:subscribe client "veron.>" (lambda (s r p)
                                             (declare (ignore s r p))))
    (let ((first-sub (wait-for-frame-matching server
                      (lambda (f) (eq (car f) :sub)))))
      (assert first-sub))
    (mock-close-current server)
    (let ((second-connect (wait-for-frame-matching server
                           (lambda (f) (eq (car f) :connect))
                           :timeout 5.0)))
      (assert second-connect))
    (let ((second-sub (wait-for-frame-matching server
                       (lambda (f) (eq (car f) :sub))
                       :timeout 3.0)))
      (assert second-sub)
      (assert (search "SUB veron.>" (second second-sub))))))

;;; Descriptors

(defun probe-descriptor-number ()
  "The descriptor a fresh socket is given, which climbs when sockets leak."
  (let ((socket (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
    (unwind-protect
         (sb-bsd-sockets:socket-file-descriptor (usocket:socket socket))
      (usocket:socket-close socket))))

(defun start-plain-server ()
  "A listener that answers in plain text, to drive a TLS handshake into failure.
Returns the port and a shutdown function. Each connection is closed as soon as
it is answered, so the server holds no descriptors of its own."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0
                                          :element-type '(unsigned-byte 8)
                                          :reuse-address t))
         (port (usocket:get-local-port listener))
         (thread (bt:make-thread
                  (lambda ()
                    (handler-case
                        (loop
                          (let ((socket (usocket:socket-accept listener)))
                            (unwind-protect
                                 (ignore-errors
                                  (let ((stream (usocket:socket-stream socket)))
                                    (write-sequence
                                     (babel:string-to-octets
                                      (format nil "INFO {}~C~C" #\Return #\Linefeed))
                                     stream)
                                    (finish-output stream)))
                              (ignore-errors (usocket:socket-close socket)))))
                      (error () nil)))
                  :name "plain-nats-server")))
    (values port
            (lambda ()
              (ignore-errors (usocket:socket-close listener))
              (ignore-errors (bt:destroy-thread thread))))))

(define-test nats-failed-tls-handshake-closes-the-socket ()
  "A server that cannot be negotiated with costs an attempt, not a descriptor."
  (multiple-value-bind (port stop) (start-plain-server)
    (unwind-protect
         (let ((before (probe-descriptor-number)))
           (dotimes (i 20)
             (handler-case
                 (veron.nats::open-socket "127.0.0.1" port :tls-p t)
               (error () nil)))
           (let ((after (probe-descriptor-number)))
             (assert (< (- after before) 10) ()
                     "Descriptor numbers climbed from ~D to ~D over 20 failed handshakes"
                     before after)))
      (funcall stop))))

(defun start-greeting-server (&key (greet-after 0.2))
  "A listener that waits, greets in the clear, and records what comes back.
Returns the port, a function giving what the client sent before the greeting
and after it, and a shutdown function."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0
                                          :element-type '(unsigned-byte 8)
                                          :reuse-address t))
         (port (usocket:get-local-port listener))
         (before (list nil))
         (after (list nil))
         (thread (bt:make-thread
                  (lambda ()
                    (handler-case
                        (let* ((socket (usocket:socket-accept listener))
                               (stream (usocket:socket-stream socket)))
                          (unwind-protect
                               (progn
                                 ;; Anything arriving in this window is the
                                 ;; client speaking before it was greeted.
                                 (sleep greet-after)
                                 (setf (first before) (listen stream))
                                 (write-sequence
                                  (babel:string-to-octets
                                   (format nil "INFO {\"tls_required\":true}~C~C"
                                           #\Return #\Linefeed))
                                  stream)
                                 (finish-output stream)
                                 (setf (first after) (read-byte stream nil nil)))
                            (ignore-errors (usocket:socket-close socket))))
                      (error () nil)))
                  :name "greeting-nats-server")))
    (values port
            (lambda () (values (first before) (first after)))
            (lambda ()
              (ignore-errors (usocket:socket-close listener))
              (ignore-errors (bt:destroy-thread thread))))))

(define-test nats-tls-starts-after-the-greeting ()
  "NATS greets in the clear and the connection is raised to TLS after it, so
nothing may be sent before the INFO line has been read."
  (multiple-value-bind (port traffic stop) (start-greeting-server)
    (unwind-protect
         (progn
           (handler-case (veron.nats::open-socket "127.0.0.1" port :tls-p t)
             (error () nil))
           (multiple-value-bind (spoke-early first-byte) (funcall traffic)
             (assert (not spoke-early) ()
                     "The client sent something before it was greeted")
             (assert (eql first-byte #x16) ()
                     "Expected a TLS handshake record (#x16) after the greeting, got ~S"
                     first-byte)))
      (funcall stop))))
