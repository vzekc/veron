;;; -*- Mode: Lisp -*-

;;; Minimal bidirectional NATS client for the VERON status bus.
;;; Implements the NATS text protocol: CONNECT, PUB, SUB, UNSUB, PING/PONG,
;;; dispatch of incoming MSG to registered handlers. Background writer and
;;; reader threads, auto-reconnect with exponential backoff, subscription
;;; re-establishment after reconnect.

(in-package #:veron.nats)

(defvar *log-function* nil
  "Optional one-arg function of a formatted string for diagnostic output.
Set by the embedding application; when NIL, messages are silently dropped.")

(defun log-message (fmt &rest args)
  (when *log-function*
    (funcall *log-function* (apply #'format nil fmt args))))

(defclass subscription ()
  ((sid :initarg :sid :reader sub-sid)
   (subject :initarg :subject :reader sub-subject)
   (queue-group :initarg :queue-group :initform nil :reader sub-queue-group)
   (handler :initarg :handler :reader sub-handler)))

(defclass nats-client ()
  ((url :initarg :url :reader client-url)
   (host :accessor client-host)
   (port :accessor client-port)
   (tls-p :accessor client-tls-p :initform nil)
   (client-name :initarg :name :initform "veron" :reader client-name)
   (socket :initform nil :accessor client-socket)
   (stream :initform nil :accessor client-stream)
   (writer-lock :initform (bt:make-lock "nats-writer") :reader client-writer-lock)
   (state :initform :disconnected :accessor client-state)
   (state-lock :initform (bt:make-lock "nats-state") :reader client-state-lock)
   (outbound :initform (sb-concurrency:make-mailbox :name "nats-outbound")
             :reader client-outbound)
   (subscriptions :initform (make-hash-table) :reader client-subscriptions)
   (subs-lock :initform (bt:make-lock "nats-subs") :reader client-subs-lock)
   (next-sid :initform 0 :accessor client-next-sid)
   (sid-lock :initform (bt:make-lock "nats-sid") :reader client-sid-lock)
   (writer-thread :initform nil :accessor client-writer-thread)
   (reader-thread :initform nil :accessor client-reader-thread)
   (running :initform nil :accessor client-running)))

;;; URL parsing

(defun parse-url (url)
  "Parse a NATS URL.
Accepts nats://[user:pass@]host[:port] (plain) or tls://... (TLS).
Returns (values host port user pass tls-p). Defaults: host=127.0.0.1, port=4222."
  (let ((s url)
        (tls-p nil))
    (cond
      ((alexandria:starts-with-subseq "tls://" s)
       (setf s (subseq s (length "tls://")) tls-p t))
      ((alexandria:starts-with-subseq "nats://" s)
       (setf s (subseq s (length "nats://")))))
    (let* ((at (position #\@ s))
           (auth (when at (subseq s 0 at)))
           (rest (if at (subseq s (1+ at)) s))
           (colon (position #\: rest))
           (host (if colon (subseq rest 0 colon) rest))
           (port (if colon (parse-integer (subseq rest (1+ colon))) 4222))
           (user nil)
           (pass nil))
      (when auth
        (let ((c (position #\: auth)))
          (if c
              (setf user (subseq auth 0 c)
                    pass (subseq auth (1+ c)))
              (setf user auth))))
      (when (zerop (length host))
        (setf host "127.0.0.1"))
      (values host port user pass tls-p))))

;;; ASCII helpers — NATS headers are always ASCII

(defun ascii-octets (s)
  (map '(simple-array (unsigned-byte 8) (*))
       (lambda (c)
         (let ((code (char-code c)))
           (unless (< code 128)
             (error "non-ASCII character in NATS header: ~S" s))
           code))
       s))

(defun octets-ascii (bytes)
  (map 'string #'code-char bytes))

(defun as-octets (payload)
  "Coerce PAYLOAD to an (unsigned-byte 8) vector.
Strings are encoded as UTF-8 via babel; byte vectors pass through."
  (etypecase payload
    (string (babel:string-to-octets payload :encoding :utf-8))
    ((vector (unsigned-byte 8)) payload)
    (array (coerce payload '(simple-array (unsigned-byte 8) (*))))))

;;; Low-level IO

(defconstant +cr+ 13)
(defconstant +lf+ 10)

(defun read-header-line (stream)
  "Read bytes until CRLF. Returns the header as a string (without CRLF).
Signals END-OF-FILE if the stream closes before CRLF is seen."
  (let ((buf (make-array 64 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0))
        (last 0))
    (loop for byte = (read-byte stream)
          do (when (and (= last +cr+) (= byte +lf+))
               (vector-pop buf)
               (return (octets-ascii buf)))
             (vector-push-extend byte buf)
             (setf last byte))))

(defun read-exact (stream n)
  "Read exactly N bytes; return as an octet vector."
  (let ((buf (make-array n :element-type '(unsigned-byte 8))))
    (loop with read = 0
          while (< read n)
          for got = (read-sequence buf stream :start read)
          do (when (= got read)
               (error 'end-of-file :stream stream))
             (setf read got))
    buf))

(defun write-all (stream octets)
  (write-sequence octets stream))

(defun write-crlf (stream)
  (write-byte +cr+ stream)
  (write-byte +lf+ stream))

;;; Frame encoding (to bytes, so we can write atomically under the lock)

(defun encode-connect-frame (client user pass)
  (let ((opts (alexandria:alist-hash-table
               `(("verbose" . ,yason:false)
                 ("pedantic" . ,yason:false)
                 ("tls_required" . ,(if (client-tls-p client) yason:true yason:false))
                 ("lang" . "common-lisp")
                 ("version" . "0.1.0")
                 ("protocol" . 1)
                 ("name" . ,(client-name client)))
               :test 'equal)))
    (when user (setf (gethash "user" opts) user))
    (when pass (setf (gethash "pass" opts) pass))
    (let ((json (with-output-to-string (s) (yason:encode opts s))))
      (concatenate '(simple-array (unsigned-byte 8) (*))
                   (ascii-octets "CONNECT ")
                   (babel:string-to-octets json :encoding :utf-8)
                   (ascii-octets (format nil "~C~C" #\Return #\Linefeed))))))

(defun encode-pub-frame (subject reply-to payload)
  (let* ((bytes (as-octets payload))
         (header (with-output-to-string (s)
                   (format s "PUB ~A " subject)
                   (when reply-to (format s "~A " reply-to))
                   (format s "~D" (length bytes))
                   (write-char #\Return s) (write-char #\Linefeed s))))
    (concatenate '(simple-array (unsigned-byte 8) (*))
                 (ascii-octets header)
                 bytes
                 (ascii-octets (format nil "~C~C" #\Return #\Linefeed)))))

(defun encode-sub-frame (subject queue-group sid)
  (let ((line (with-output-to-string (s)
                (format s "SUB ~A " subject)
                (when queue-group (format s "~A " queue-group))
                (format s "~D" sid)
                (write-char #\Return s) (write-char #\Linefeed s))))
    (ascii-octets line)))

(defun encode-unsub-frame (sid max-msgs)
  (let ((line (with-output-to-string (s)
                (format s "UNSUB ~D" sid)
                (when max-msgs (format s " ~D" max-msgs))
                (write-char #\Return s) (write-char #\Linefeed s))))
    (ascii-octets line)))

(defun encode-pong-frame ()
  (ascii-octets (format nil "PONG~C~C" #\Return #\Linefeed)))

;;; Outbound message representation — plists:
;;;   (:type :pub :subject … :reply-to … :payload …)
;;;   (:type :sub :subject … :queue-group … :sid …)
;;;   (:type :unsub :sid … :max-msgs …)
;;;   (:type :pong)
;;;   (:type :stop)

(defun encode-item (item)
  (ecase (getf item :type)
    (:pub (encode-pub-frame (getf item :subject)
                            (getf item :reply-to)
                            (getf item :payload)))
    (:sub (encode-sub-frame (getf item :subject)
                            (getf item :queue-group)
                            (getf item :sid)))
    (:unsub (encode-unsub-frame (getf item :sid) (getf item :max-msgs)))
    (:pong (encode-pong-frame))))

;;; Connection lifecycle

(defun open-socket (host port &key tls-p)
  "Open a connection and read the server's greeting.
Returns (values socket stream info).

A NATS server speaks its INFO line in the clear and the connection is raised to
TLS after it, so a TLS connection is made in that order: the greeting is read
from the bare socket and the stream handed back is the encrypted one, which
everything from CONNECT onwards travels over.

The socket is closed if any of that fails, so that a server which cannot be
negotiated with costs one attempt rather than one file descriptor a retry."
  (let ((socket (usocket:socket-connect host port
                                        :element-type '(unsigned-byte 8))))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (close-socket-quietly socket))))
      (let* ((raw (usocket:socket-stream socket))
             (info (read-header-line raw))
             (stream (if tls-p
                         (cl+ssl:make-ssl-client-stream raw :hostname host
                                                            :verify :required)
                         raw)))
        (values socket stream info)))))

(defun close-socket-quietly (socket)
  (when socket
    (ignore-errors (usocket:socket-close socket))))

(defun set-state (client new-state)
  (bt:with-lock-held ((client-state-lock client))
    (setf (client-state client) new-state)))

(defun perform-handshake (client info)
  "Check the server's greeting and send CONNECT over the established stream."
  (unless (alexandria:starts-with-subseq "INFO " info)
    (error "expected INFO from server, got: ~A" info))
  (multiple-value-bind (host port user pass)
      (parse-url (client-url client))
    (declare (ignore host port))
    (write-all (client-stream client) (encode-connect-frame client user pass))
    (finish-output (client-stream client))))

(defun resend-subscriptions (client)
  (bt:with-lock-held ((client-subs-lock client))
    (maphash (lambda (sid sub)
               (declare (ignore sid))
               (sb-concurrency:send-message
                (client-outbound client)
                (list :type :sub
                      :subject (sub-subject sub)
                      :queue-group (sub-queue-group sub)
                      :sid (sub-sid sub))))
             (client-subscriptions client))))

(defun handle-msg-line (client header)
  "Parse a MSG header line and read the payload, then dispatch."
  (let* ((parts (split-sequence-space header))
         (n (length parts))
         (subject (first parts))
         (sid (parse-integer (second parts)))
         (reply-to (when (= n 4) (third parts)))
         (size (parse-integer (if (= n 4) (fourth parts) (third parts))))
         (payload (read-exact (client-stream client) size)))
    (read-exact (client-stream client) 2)
    (let ((sub (bt:with-lock-held ((client-subs-lock client))
                 (gethash sid (client-subscriptions client)))))
      (when sub
        (handler-case
            (funcall (sub-handler sub) subject reply-to payload)
          (error (e)
            (log-message "nats handler error on ~A: ~A" subject e)))))))

(defun split-sequence-space (s)
  (loop with start = 0
        with len = (length s)
        while (< start len)
        for space = (position #\Space s :start start)
        collect (subseq s start (or space len))
        do (setf start (if space (1+ space) len))))

(defun dispatch-frame (client line)
  (cond
    ((zerop (length line)))
    ((alexandria:starts-with-subseq "+OK" line))
    ((alexandria:starts-with-subseq "-ERR" line)
     (log-message "nats -ERR: ~A" line))
    ((alexandria:starts-with-subseq "MSG " line)
     (handle-msg-line client (subseq line 4)))
    ((alexandria:starts-with-subseq "INFO " line))
    (t
     (let* ((space (position #\Space line))
            (verb (find-symbol (string-upcase (subseq line 0 (or space (length line))))
                               :keyword)))
       (case verb
         (:ping (sb-concurrency:send-message (client-outbound client)
                                             '(:type :pong)))
         (:pong nil)
         (t (log-message "nats unknown frame: ~A" line)))))))

(defun reader-loop (client)
  (handler-case
      (loop while (client-running client)
            do (dispatch-frame client (read-header-line (client-stream client))))
    (error (e)
      (when (client-running client)
        (log-message "nats reader error: ~A" e)
        (set-state client :disconnected)
        (sb-concurrency:send-message (client-outbound client)
                                     '(:type :wake))))))

(defun attempt-reconnect (client)
  "Connect the socket, perform handshake, start the reader thread,
and re-subscribe. Signals on failure."
  (let ((greeting nil))
    (multiple-value-bind (host port user pass tls-p) (parse-url (client-url client))
      (declare (ignore user pass))
      (multiple-value-bind (socket stream info) (open-socket host port :tls-p tls-p)
        (setf (client-host client) host
              (client-port client) port
              (client-tls-p client) tls-p
              (client-socket client) socket
              (client-stream client) stream
              greeting info)))
    (perform-handshake client greeting))
  (set-state client :connected)
  (let ((reader (bt:make-thread (lambda () (reader-loop client))
                                :name "nats-reader")))
    (setf (client-reader-thread client) reader))
  (resend-subscriptions client))

(defun teardown-connection (client)
  (set-state client :disconnected)
  (let ((reader (client-reader-thread client)))
    (when (and reader (bt:thread-alive-p reader)
               (not (eq reader (bt:current-thread))))
      (ignore-errors (bt:destroy-thread reader))))
  (close-socket-quietly (client-socket client))
  (setf (client-socket client) nil
        (client-stream client) nil
        (client-reader-thread client) nil))

(defun writer-loop (client)
  (let ((backoff 1))
    (loop while (client-running client) do
      (unless (eq (client-state client) :connected)
        (handler-case
            (progn
              (attempt-reconnect client)
              (setf backoff 1)
              (log-message "nats connected to ~A:~D"
                           (client-host client) (client-port client)))
          (error (e)
            (log-message "nats connect failed (~A); retrying in ~Ds" e backoff)
            (teardown-connection client)
            (sleep backoff)
            (setf backoff (min (* backoff 2) 15)))))
      (when (eq (client-state client) :connected)
        (let ((item (sb-concurrency:receive-message (client-outbound client))))
          (when item
            (case (getf item :type)
              (:stop (return-from writer-loop))
              (:wake nil)
              (t
               (handler-case
                   (bt:with-lock-held ((client-writer-lock client))
                     (write-all (client-stream client) (encode-item item))
                     (finish-output (client-stream client)))
                 (error (e)
                   (log-message "nats write error (~A); reconnecting" e)
                   (unless (eq (getf item :type) :pong)
                     (sb-concurrency:send-message (client-outbound client) item))
                   (teardown-connection client)))))))))))

;;; Public API

(defun connect (url &key (name "veron"))
  "Open a NATS connection to URL and start the background threads.
Returns a NATS-CLIENT. Publishes issued before the socket comes up are
queued and flushed on first successful connect."
  (let ((client (make-instance 'nats-client :url url :name name)))
    (setf (client-running client) t)
    (setf (client-writer-thread client)
          (bt:make-thread (lambda () (writer-loop client)) :name "nats-writer"))
    client))

(defun disconnect (client)
  "Stop the client's background threads and close the socket."
  (setf (client-running client) nil)
  (sb-concurrency:send-message (client-outbound client) '(:type :stop))
  (let ((writer (client-writer-thread client)))
    (when (and writer (bt:thread-alive-p writer)
               (not (eq writer (bt:current-thread))))
      (ignore-errors (bt:join-thread writer))))
  (teardown-connection client))

(defun connected-p (client)
  (eq (client-state client) :connected))

(defun publish (client subject payload &key reply-to)
  "Queue a PUB frame. PAYLOAD is a string or octet vector.
Returns immediately; never blocks waiting for the socket."
  (sb-concurrency:send-message
   (client-outbound client)
   (list :type :pub :subject subject :reply-to reply-to :payload payload)))

(defun next-sid (client)
  (bt:with-lock-held ((client-sid-lock client))
    (incf (client-next-sid client))))

(defun subscribe (client subject handler &key queue-group)
  "Register HANDLER for messages on SUBJECT and send SUB. Returns an sid.
HANDLER is called on the reader thread as (handler subject reply-to payload);
it must not block."
  (let* ((sid (next-sid client))
         (sub (make-instance 'subscription
                             :sid sid
                             :subject subject
                             :queue-group queue-group
                             :handler handler)))
    (bt:with-lock-held ((client-subs-lock client))
      (setf (gethash sid (client-subscriptions client)) sub))
    (sb-concurrency:send-message
     (client-outbound client)
     (list :type :sub :subject subject :queue-group queue-group :sid sid))
    sid))

(defun unsubscribe (client sid &key max-msgs)
  "Send UNSUB. If MAX-MSGS is NIL, removes the handler immediately;
otherwise leaves it registered until MAX-MSGS messages have been delivered."
  (sb-concurrency:send-message
   (client-outbound client)
   (list :type :unsub :sid sid :max-msgs max-msgs))
  (unless max-msgs
    (bt:with-lock-held ((client-subs-lock client))
      (remhash sid (client-subscriptions client)))))
