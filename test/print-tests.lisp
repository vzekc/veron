;;; -*- Mode: Lisp -*-

;;; Tests for Fotodruck: the relay connection, and what the screen says when
;;; there is no printer.

(in-package #:veron-tests)

(defun wait-until (predicate &key (timeout 5.0))
  "Poll PREDICATE until it returns true. Returns T, or NIL on timeout."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (when (funcall predicate)
        (return t))
      (when (> (get-internal-real-time) deadline)
        (return nil))
      (bt:thread-yield))))

(defun free-port ()
  "An unused local port."
  (let ((socket (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
    (unwind-protect (usocket:get-local-port socket)
      (usocket:socket-close socket))))

(defmacro with-print-listener ((port-var &key (token "test-token")) &body body)
  "Run BODY with the Fotodruck listener up on a free port, then shut it down."
  `(let ((,port-var (free-port)))
     (setf (uiop:getenv "VERON_PRINT_PORT") (princ-to-string ,port-var)
           (uiop:getenv "VERON_PRINT_TOKEN") ,token)
     (unwind-protect
          (progn
            (veron::start-print-listener :host "127.0.0.1")
            ,@body)
       (veron::stop-print-listener)
       (dolist (printer veron::*printers*)
         (let ((relay (veron::printer-relay printer)))
           (when relay
             (ignore-errors (usocket:socket-close relay))))
         (setf (veron::printer-relay printer) nil
               (veron::printer-job printer) nil))
       (setf (uiop:getenv "VERON_PRINT_PORT") ""
             (uiop:getenv "VERON_PRINT_TOKEN") ""))))

(defun connect-relay (port line)
  "Connect to the listener on PORT and sign on with LINE."
  (let ((socket (usocket:socket-connect "127.0.0.1" port
                                        :element-type '(unsigned-byte 8))))
    (write-sequence (babel:string-to-octets (format nil "~A~%" line))
                    (usocket:socket-stream socket))
    (force-output (usocket:socket-stream socket))
    socket))

(defun wait-for-close (socket &key (timeout 5.0))
  "T when the far end closes the connection within TIMEOUT."
  (let ((stream (usocket:socket-stream socket))
        (deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (when (> (get-internal-real-time) deadline)
        (return nil))
      ;; Only read once something is there, so a connection left open times
      ;; out here rather than blocking in read-byte.
      (when (or (listen stream)
                (usocket:wait-for-input socket :timeout 1 :ready-only t))
        (return (null (handler-case (read-byte stream nil nil)
                        (error () nil))))))))

(defun read-until-eof (socket &key (timeout 20.0))
  "Read everything the relay is sent, until the run ends with a close.
Gives up after TIMEOUT, so a connection that is never closed fails the test
that reads it instead of hanging the suite."
  (let ((stream (usocket:socket-stream socket))
        (deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second)))
        (received (make-array 0 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer t)))
    (handler-case
        (loop
          (when (> (get-internal-real-time) deadline)
            (return))
          (when (or (listen stream)
                    (usocket:wait-for-input socket :timeout 1 :ready-only t))
            (let ((byte (read-byte stream nil nil)))
              (unless byte
                (return))
              (vector-push-extend byte received))))
      (error () nil))
    received))

;;; A stand-in for the exhibition website

(defparameter *crlf* (coerce (list #\Return #\Newline) 'string))

(defun read-http-line (stream)
  "One CRLF-terminated line, or NIL at end of stream."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer t)))
    (loop for byte = (read-byte stream nil nil)
          do (cond ((null byte) (return (when (plusp (fill-pointer bytes))
                                          (babel:octets-to-string
                                           (coerce bytes '(vector (unsigned-byte 8)))))))
                   ((= byte 10) (return (babel:octets-to-string
                                         (coerce bytes '(vector (unsigned-byte 8))))))
                   ((= byte 13))
                   (t (vector-push-extend byte bytes))))))

(defun serve-one-http-request (socket responder)
  (let* ((stream (usocket:socket-stream socket))
         (request-line (read-http-line stream)))
    (when request-line
      (loop for line = (read-http-line stream)
            until (or (null line) (string= line "")))
      (let* ((start (1+ (position #\Space request-line)))
             (path (subseq request-line start (position #\Space request-line :start start))))
        (multiple-value-bind (status body) (funcall responder path)
          (write-sequence
           (babel:string-to-octets
            (format nil "HTTP/1.1 ~D ~A~AContent-Length: ~D~AConnection: close~A~A"
                    status (if (= status 200) "OK" "Not Found") *crlf*
                    (length body) *crlf* *crlf* *crlf*))
           stream)
          (write-sequence body stream)
          (force-output stream))))))

(defun start-photo-website (responder)
  "Serve RESPONDER's answers over HTTP. Returns the port and a shutdown function.
RESPONDER receives the request path and returns a status and the body as octets."
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
                                 (ignore-errors (serve-one-http-request socket responder))
                              (ignore-errors (usocket:socket-close socket)))))
                      (error () nil)))
                  :name "test-photo-website")))
    (values port
            (lambda ()
              (ignore-errors (usocket:socket-close listener))
              (ignore-errors (bt:destroy-thread thread))))))

;;; The relay connection

(define-test print-relay-signs-on ()
  "A relay with the right token makes its printer available."
  (with-print-listener (port)
    (let ((printer (veron::find-printer "nec-p6"))
          (socket (connect-relay port (format nil "nec-p6 ~A" "test-token"))))
      (unwind-protect
           (progn
             (assert (wait-until (lambda () (veron::printer-relay printer))) ()
                     "Relay should have been registered")
             (assert (veron::printer-ready-p printer) ()
                     "Printer should be ready once the relay is connected")
             (assert (equal (list printer) (veron::ready-printers)) ()
                     "The connected printer should be the only ready one"))
        (usocket:socket-close socket)))))

(define-test print-registration-keeps-the-relay ()
  "Re-registering a printer, as a hot reload does, leaves its relay connected."
  (with-print-listener (port)
    (let ((printer (veron::find-printer "nec-p6"))
          (socket (connect-relay port "nec-p6 test-token")))
      (unwind-protect
           (progn
             (assert (wait-until (lambda () (veron::printer-ready-p printer))) ()
                     "Printer should be ready")
             (veron::register-printer "nec-p6" "NEC Pinwriter P6"
                                      (list (veron::resolution 60 "Niedrig" 3)))
             (assert (eq printer (veron::find-printer "nec-p6")) ()
                     "Re-registering should keep the same printer")
             (assert (veron::printer-ready-p (veron::find-printer "nec-p6")) ()
                     "The relay should still be connected after re-registration")
             (assert (= 1 (length (veron::printer-resolutions printer))) ()
                     "Re-registering should update what the printer offers"))
        (ignore-errors (usocket:socket-close socket))
        ;; Put the table back the way the source defines it.
        (veron::register-printer "nec-p6" "NEC Pinwriter P6"
                                 (list (veron::resolution 360 "Hoch" 99)
                                       (veron::resolution 180 "Mittel" 99)
                                       (veron::resolution 60 "Niedrig" 99)))))))

(define-test print-relay-rejects-wrong-token ()
  "A relay with the wrong token is dropped and no printer becomes available."
  (with-print-listener (port)
    (let ((printer (veron::find-printer "nec-p6"))
          (socket (connect-relay port "nec-p6 falsch")))
      (unwind-protect
           (progn
             (assert (wait-for-close socket) () "Connection should have been closed")
             (assert (null (veron::printer-relay printer)) ()
                     "No relay should be registered after a wrong token"))
        (ignore-errors (usocket:socket-close socket))))))

(define-test print-relay-rejects-unknown-printer ()
  "A relay signing on for a printer that is not configured is dropped."
  (with-print-listener (port)
    (let ((socket (connect-relay port "kein-drucker test-token")))
      (unwind-protect
           (assert (wait-for-close socket) () "Connection should have been closed")
        (ignore-errors (usocket:socket-close socket))))))

;;; A run on its way out

(define-test print-job-reaches-the-relay ()
  "Every byte of the run arrives at the relay, and the close ends the job."
  (with-print-listener (port)
    (let* ((printer (veron::find-printer "nec-p6"))
           (resolution (veron::find-resolution printer 180))
           (data (make-array 10000 :element-type '(unsigned-byte 8)))
           (socket (connect-relay port "nec-p6 test-token")))
      (dotimes (i (length data))
        (setf (aref data i) (mod (* i 7) 256)))
      (unwind-protect
           (progn
             (assert (wait-until (lambda () (veron::printer-ready-p printer))) ()
                     "Printer should be ready")
             (let ((job (veron::start-print-job printer resolution "testuser" data)))
               (assert job () "The job should have been accepted")
               (let ((received (read-until-eof socket)))
                 (assert (= (length data) (length received)) ()
                         "Expected ~D bytes at the relay, got ~D"
                         (length data) (length received))
                 (assert (every #'= data received) ()
                         "The run arrived altered"))
               (assert (wait-until (lambda () (eq :done (veron::print-job-state job)))) ()
                       "The job should have finished, state is ~S"
                       (veron::print-job-state job))
               (assert (null (veron::printer-relay printer)) ()
                       "The relay should be detached once the run has gone out")))
        (ignore-errors (usocket:socket-close socket))))))

(define-test print-job-refused-while-busy ()
  "One sheet at a time: a second job is refused while the first is running."
  (with-print-listener (port)
    (let* ((printer (veron::find-printer "nec-p6"))
           (resolution (veron::find-resolution printer 180))
           (data (make-array 100 :element-type '(unsigned-byte 8) :initial-element 65))
           (socket (connect-relay port "nec-p6 test-token")))
      (unwind-protect
           (progn
             (assert (wait-until (lambda () (veron::printer-ready-p printer))) ()
                     "Printer should be ready")
             ;; Claim the printer without sending, so the job stays active.
             (let ((job (make-instance 'veron::print-job
                                       :printer printer :resolution resolution
                                       :username "testuser" :data data
                                       :total (length data))))
               (setf (veron::print-job-state job) :running
                     (veron::printer-job printer) job)
               (assert (not (veron::printer-ready-p printer)) ()
                       "A printer with a running job is not ready")
               (assert (null (veron::start-print-job printer resolution "otheruser" data)) ()
                       "A second job should be refused while one is running")))
        (ignore-errors (usocket:socket-close socket))))))

;;; Photo ids

(define-test print-photo-id-validation ()
  "Six characters from the booth's alphabet, and nothing else."
  (assert (veron::valid-photo-id-p "K7NP4M") () "A well-formed id should be accepted")
  (assert (not (veron::valid-photo-id-p "K7NP4")) () "Five characters is too few")
  (assert (not (veron::valid-photo-id-p "K7NP4MM")) () "Seven characters is too many")
  (assert (not (veron::valid-photo-id-p "K7NP4O")) () "O is not in the alphabet")
  (assert (not (veron::valid-photo-id-p "K7NP4I")) () "I is not in the alphabet")
  (assert (not (veron::valid-photo-id-p "K7NP41")) () "1 is not in the alphabet")
  (assert (not (veron::valid-photo-id-p "k7np4m")) () "Lower case is not an id"))

;;; The screen

(define-test e2e-fotodruck-unconfigured ()
  "Without a listener the item is still on the menu and says so."
  (with-veron-app (s :username "printuser" :password "printpass")
    (login s "printuser" "printpass")
    (assert-on-screen s "MAIN")
    (select-menu-item s "Fotodruck")
    (assert-on-screen s "FOTODRUCK")
    (assert (wait-for-screen-contains s "nicht konfiguriert" :timeout 3)
            () "Should say Fotodruck is not configured")))

(define-test e2e-fotodruck-no-relay ()
  "Configured but with nothing connected, the screen does not ask for an id."
  (with-print-listener (port)
    (assert (plusp port) () "The listener should have a port")
    (with-veron-app (s :username "printuser2" :password "printpass2")
      (login s "printuser2" "printpass2")
      (select-menu-item s "Fotodruck")
      (assert-on-screen s "FOTODRUCK")
      (assert (wait-for-screen-contains s "kein Drucker verbunden" :timeout 3)
              () "Should say no printer is connected")
      (assert (not (search "Foto-ID" (format nil "~{~A~^~%~}" (screen-text s))))
              () "Should not ask for a photo id when there is no printer"))))

(defun test-print-run (length)
  (let ((data (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length data)
      (setf (aref data i) (mod (* i 7) 256)))))

(defmacro with-photo-website ((responder) &body body)
  "Point VERON_FOTOFIX_URL at a website that answers with RESPONDER."
  (let ((stop (gensym "STOP")) (port (gensym "PORT")))
    `(multiple-value-bind (,port ,stop) (start-photo-website ,responder)
       (setf (uiop:getenv "VERON_FOTOFIX_URL") (format nil "http://127.0.0.1:~D" ,port))
       (unwind-protect (progn ,@body)
         (funcall ,stop)
         (setf (uiop:getenv "VERON_FOTOFIX_URL") "https://2026.classic-computing.de")))))

(define-test e2e-fotodruck-prints-a-photo ()
  "A photo id and a density typed in reach the relay as the run for that file."
  (with-print-listener (port)
    (let* ((run (test-print-run 20000))
           (printer (veron::find-printer "nec-p6"))
           (relay (connect-relay port "nec-p6 test-token")))
      (with-photo-website ((lambda (path)
                             (if (string= path "/foto/K7NP4M/nec-p6-180.prn")
                                 (values 200 run)
                                 (values 404 (babel:string-to-octets "no")))))
        (unwind-protect
             (progn
               (assert (wait-until (lambda () (veron::printer-ready-p printer))) ()
                       "Printer should be ready")
               (with-veron-app (s :username "printuser4" :password "printpass4")
                 (login s "printuser4" "printpass4")
                 (select-menu-item s "Fotodruck")
                 (assert (wait-for-screen-contains s "Foto-ID" :timeout 3)
                         () "Should ask for the photo id")
                 (move-cursor s 8 14)
                 (type-text s "K7NP4M")
                 (move-cursor s 10 14)
                 (type-text s "2")
                 (press-enter s)
                 (assert-on-screen s "FOTODRUCK-STATUS")
                 (let ((received (read-until-eof relay)))
                   (assert (= (length run) (length received)) ()
                           "Expected ~D bytes at the relay, got ~D"
                           (length run) (length received))
                   (assert (every #'= run received) () "The run arrived altered"))
                 (assert (wait-for-screen-contains s "Der Ausdruck ist fertig" :timeout 5)
                         () "The status screen should report the sheet as finished")
                 (let ((full (format nil "~{~A~^~%~}" (screen-text s))))
                   (assert (search "NEC Pinwriter P6" full) () "Should name the printer")
                   (assert (search "180 dpi" full) () "Should name the density"))))
          (ignore-errors (usocket:socket-close relay)))))))

(define-test e2e-fotodruck-unknown-id ()
  "An id the website does not know is refused before anything is printed."
  (with-print-listener (port)
    (let ((printer (veron::find-printer "nec-p6"))
          (relay (connect-relay port "nec-p6 test-token")))
      (with-photo-website ((lambda (path)
                             (declare (ignore path))
                             (values 404 (babel:string-to-octets "no"))))
        (unwind-protect
             (progn
               (assert (wait-until (lambda () (veron::printer-ready-p printer))) ()
                       "Printer should be ready")
               (with-veron-app (s :username "printuser5" :password "printpass5")
                 (login s "printuser5" "printpass5")
                 (select-menu-item s "Fotodruck")
                 (assert (wait-for-screen-contains s "Foto-ID" :timeout 3)
                         () "Should ask for the photo id")
                 (move-cursor s 8 14)
                 (type-text s "K7NP4M")
                 (move-cursor s 10 14)
                 (type-text s "2")
                 (press-enter s)
                 (assert-on-screen s "FOTODRUCK")
                 (assert (wait-for-screen-contains s "unbekannt" :timeout 3)
                         () "Should say the photo id is unknown")
                 (assert (veron::printer-ready-p printer) ()
                         "The printer should not have been claimed")))
          (ignore-errors (usocket:socket-close relay)))))))

(define-test e2e-fotodruck-still-converting ()
  "A photo whose formats are not made yet is told apart from an unknown id."
  (with-print-listener (port)
    (let ((printer (veron::find-printer "nec-p6"))
          (relay (connect-relay port "nec-p6 test-token")))
      (with-photo-website ((lambda (path)
                             (if (string= path "/api/visitor-photo/K7NP4M/page")
                                 (values 200 (babel:string-to-octets
                                              "{\"id\":\"K7NP4M\",\"deleted\":false,\"converting\":true}"))
                                 (values 404 (babel:string-to-octets "no")))))
        (unwind-protect
             (progn
               (assert (wait-until (lambda () (veron::printer-ready-p printer))) ()
                       "Printer should be ready")
               (with-veron-app (s :username "printuser6" :password "printpass6")
                 (login s "printuser6" "printpass6")
                 (select-menu-item s "Fotodruck")
                 (assert (wait-for-screen-contains s "Foto-ID" :timeout 3)
                         () "Should ask for the photo id")
                 (move-cursor s 8 14)
                 (type-text s "K7NP4M")
                 (move-cursor s 10 14)
                 (type-text s "2")
                 (press-enter s)
                 (assert (wait-for-screen-contains s "aufbereitet" :timeout 3)
                         () "Should say the photo is still being converted")))
          (ignore-errors (usocket:socket-close relay)))))))

(define-test e2e-fotodruck-offers-the-form ()
  "With a relay connected the form asks for an id and offers the densities."
  (with-print-listener (port)
    (let ((printer (veron::find-printer "nec-p6"))
          (socket (connect-relay port "nec-p6 test-token")))
      (unwind-protect
           (progn
             (assert (wait-until (lambda () (veron::printer-ready-p printer))) ()
                     "Printer should be ready")
             (with-veron-app (s :username "printuser3" :password "printpass3")
               (login s "printuser3" "printpass3")
               (select-menu-item s "Fotodruck")
               (assert-on-screen s "FOTODRUCK")
               (assert (wait-for-screen-contains s "Foto-ID" :timeout 3)
                       () "Should ask for the photo id")
               (let ((full (format nil "~{~A~^~%~}" (screen-text s))))
                 (assert (search "Hoch" full) () "Should offer the high density")
                 (assert (search "360 dpi" full) () "Should name the density")
                 (assert (search "NEC Pinwriter P6" full) ()
                         "Should name the connected printer"))))
        (ignore-errors (usocket:socket-close socket))))))
