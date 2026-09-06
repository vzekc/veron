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

(defun register-test-printer ()
  "The show's printer with printing times of a few seconds, so that a run paced
against the sheet finishes while a test is watching."
  (veron::register-printer "nec-p6" "NEC Pinwriter P6"
                           (list (veron::resolution 60 "Niedrig" 1)
                                 (veron::resolution 180 "Mittel" 2)
                                 (veron::resolution 360 "Hoch" 3))))

(defmacro with-print-listener ((port-var &key (token "test-token")) &body body)
  "Run BODY with the Fotodruck listener up on a free port, then shut it down.
The printer it offers is the test one, and the run it waits out at the end is
shortened to match, so a sheet takes seconds rather than minutes."
  `(let ((,port-var (free-port))
         (drain veron::*print-drain-seconds*))
     (setf (uiop:getenv "VERON_PRINT_PORT") (princ-to-string ,port-var)
           (uiop:getenv "VERON_PRINT_TOKEN") ,token
           veron::*print-drain-seconds* 1)
     (register-test-printer)
     (unwind-protect
          (progn
            (veron::start-print-listener :host "127.0.0.1")
            ,@body)
       (veron::stop-print-listener)
       (setf veron::*print-drain-seconds* drain)
       (dolist (printer veron::*printers*)
         (let ((relay (veron::printer-relay printer)))
           (when relay
             (ignore-errors (usocket:socket-close relay))))
         (setf (veron::printer-relay printer) nil
               (veron::printer-job printer) nil))
       (veron::register-show-printers)
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

(define-test print-relay-connection-is-probed ()
  "The connection is kept under keepalive, so a relay that goes away without a
close does not leave the printer looking connected for the life of the process."
  (with-print-listener (port)
    (let ((printer (veron::find-printer "nec-p6"))
          (socket (connect-relay port "nec-p6 test-token")))
      (unwind-protect
           (progn
             (assert (wait-until (lambda () (veron::printer-relay printer))) ()
                     "Relay should have been registered")
             (assert (sb-bsd-sockets:sockopt-keep-alive
                      (usocket:socket (veron::printer-relay printer)))
                     () "Keepalive should be enabled on the relay connection"))
        (ignore-errors (usocket:socket-close socket))))))

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
        (ignore-errors (usocket:socket-close socket))))))

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

(define-test print-run-takes-the-sheets-time ()
  "The run goes out at the pace of the sheet, so a small one is not over at once."
  (with-print-listener (port)
    (let* ((printer (veron::find-printer "nec-p6"))
           (resolution (veron::find-resolution printer 180))
           (data (make-array 20000 :element-type '(unsigned-byte 8) :initial-element 65))
           (socket (connect-relay port "nec-p6 test-token")))
      (unwind-protect
           (progn
             (assert (wait-until (lambda () (veron::printer-ready-p printer))) ()
                     "Printer should be ready")
             (let ((started (get-internal-real-time))
                   (job (veron::start-print-job printer resolution "testuser" data)))
               (assert job () "The job should have been accepted")
               (assert (wait-until (lambda () (plusp (veron::print-job-sent job)))) ()
                       "The run should have started")
               (assert (< (veron::print-job-sent job) (length data)) ()
                       "The whole run should not have gone out in one go")
               (assert (= (length data) (length (read-until-eof socket))) ()
                       "Every byte should reach the relay")
               (assert (wait-until (lambda () (eq :done (veron::print-job-state job)))) ()
                       "The job should have finished, state is ~S"
                       (veron::print-job-state job))
               (assert (>= (/ (- (get-internal-real-time) started)
                              internal-time-units-per-second)
                           1)
                       () "The run should have taken about as long as the sheet")))
        (ignore-errors (usocket:socket-close socket))))))

(define-test print-printer-busy-until-the-relay-returns ()
  "A printer whose run has just ended is busy, not gone, until its relay is back."
  (let* ((printer (veron::find-printer "nec-p6"))
         (resolution (veron::find-resolution printer 180))
         (job (make-instance 'veron::print-job
                             :printer printer :resolution resolution
                             :username "testuser" :data nil :total 1000)))
    (unwind-protect
         (progn
           (setf (veron::printer-relay printer) nil
                 (veron::printer-job printer) job
                 (veron::print-job-state job) :running)
           (assert (veron::printer-busy-p printer) () "A running job makes a printer busy")
           (veron::finish-print-job job :done nil)
           (assert (veron::printer-busy-p printer) ()
                   "A printer whose relay has yet to dial back in is still busy")
           (setf (veron::print-job-finished-at job)
                 (- (get-universal-time) veron::*relay-return-seconds* 1))
           (assert (not (veron::printer-busy-p printer)) ()
                   "A printer whose relay never came back counts as gone"))
      (setf (veron::printer-job printer) nil))))

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

;;; What the show offers

(define-test print-duration-text ()
  "A printing time reads the way the screens say it."
  (assert (string= "1 Minute" (veron::print-duration-text 60)) () "One whole minute")
  (assert (string= "5 Minuten" (veron::print-duration-text 300)) () "Whole minutes")
  (assert (string= "3:30 Minuten" (veron::print-duration-text 210)) () "Part minutes")
  (assert (string= "45 Sekunden" (veron::print-duration-text 45)) () "Under a minute"))

(define-test print-densities-in-order ()
  "The show's densities run from coarse to fine, with the times measured on it."
  (veron::register-show-printers)
  (let ((resolutions (veron::printer-resolutions (veron::find-printer "nec-p6"))))
    (assert (equal '("Niedrig" "Mittel" "Hoch")
                   (mapcar #'veron::print-resolution-label resolutions))
            () "Densities should be offered coarsest first")
    (assert (equal '(60 180 360) (mapcar #'veron::print-resolution-dpi resolutions))
            () "Labels should go with the densities they name")
    (assert (equal '("1 Minute" "3:30 Minuten" "5 Minuten")
                   (mapcar (lambda (r)
                             (veron::print-duration-text
                              (veron::print-resolution-seconds r)))
                           resolutions))
            () "Printing times should be the ones the show measured")))

;;; What the visitor is told about a run

(defun make-test-job (&key (dpi 180) (sent 0) (total 1000))
  "A job on the show's printer, with SENT of TOTAL bytes gone out."
  (veron::register-show-printers)
  (let* ((printer (veron::find-printer "nec-p6"))
         (job (make-instance 'veron::print-job
                             :printer printer
                             :resolution (veron::find-resolution printer dpi)
                             :username "testuser" :data nil :total total)))
    (setf (veron::print-job-sent job) sent)
    job))

(define-test print-estimate-waits-for-a-measurement ()
  "Nothing is claimed about a run's pace until it has been going long enough."
  (let ((job (make-test-job :sent 500)))
    (assert (null (veron::print-job-remaining-seconds job)) ()
            "A run that has just started has nothing to say about its pace")
    (let ((veron::*estimate-warmup-seconds* 0))
      (assert (veron::print-job-remaining-seconds job) ()
              "A run past the warmup should be given a time"))))

(define-test print-estimate-in-quarter-minutes ()
  "An estimate is rounded up to a step and carries the run's own tail."
  (let ((veron::*estimate-warmup-seconds* 0))
    (let* ((job (make-test-job :sent 500))
           (sheet (veron::print-job-seconds job))
           (remaining (veron::print-job-remaining-seconds job)))
      (assert (zerop (mod remaining veron::*estimate-step-seconds*)) ()
              "~D should be a whole number of steps" remaining)
      (assert (>= remaining (+ (/ sheet 2) veron::*print-drain-seconds*)) ()
              "~D should cover the half sheet left and the tail" remaining))
    (let ((job (make-test-job :sent 1000)))
      (assert (>= (veron::print-job-remaining-seconds job)
                  veron::*estimate-step-seconds*)
              () "A run whose writing is over still has its tail to go"))))

(define-test print-busy-note-follows-the-run ()
  "What a second visitor is told follows how far the sheet has got."
  (let* ((job (make-test-job :sent 500))
         (printer (veron::print-job-printer job)))
    (unwind-protect
         (progn
           (setf (veron::printer-job printer) job
                 (veron::print-job-state job) :running)
           (assert (search "gemessen" (veron::busy-printer-note printer)) ()
                   "A run too young to judge should say so, got ~S"
                   (veron::busy-printer-note printer))
           (let ((veron::*estimate-warmup-seconds* 0))
             (assert (search "in ca." (veron::busy-printer-note printer)) ()
                     "A run past the warmup should be given a time, got ~S"
                     (veron::busy-printer-note printer)))
           (setf (veron::print-job-state job) :draining)
           (assert (search "gleich fertig" (veron::busy-printer-note printer)) ()
                   "The tail of a run should read as nearly done, got ~S"
                   (veron::busy-printer-note printer)))
      (setf (veron::printer-job printer) nil))))

(define-test print-status-says-nearly-done ()
  "The tail of a run is reported as nearly done, and without a time."
  (let ((job (make-test-job :sent 1000)))
    (setf (veron::print-job-state job) :draining)
    (let ((lines (format nil "~{~A~^~%~}" (veron::print-status-lines job))))
      (assert (search "gleich fertig" lines) ()
              "Should say the sheet is nearly done, got ~S" lines)
      (assert (not (search "Fertig in" lines)) ()
              "Should not offer a time once the writing is over, got ~S" lines))
    (setf (veron::print-job-state job) :done)
    (let ((lines (format nil "~{~A~^~%~}" (veron::print-status-lines job))))
      (assert (search "Der Ausdruck ist fertig" lines) ()
              "A finished run should say so, got ~S" lines))))

(define-test print-run-error-names-a-field ()
  "Every reason a run cannot be fetched has something to say and a field to
say it in, so the field order is not what decides where a correction is typed."
  (dolist (reason '(:unknown :deleted :converting :unreachable nil))
    (multiple-value-bind (message field) (veron::print-run-error reason)
      (assert (plusp (length message)) () "~S should have something to say" reason)
      (assert (string= "photo-id" field) ()
              "~S is about the id, but names ~S" reason field)))
  (multiple-value-bind (message field) (veron::print-run-error :missing)
    (assert (plusp (length message)) () ":missing should have something to say")
    (assert (string= "res-sel" field) ()
            ":missing is about the density, but names ~S" field)))

;;; Photo ids

(define-test print-preview-fits-the-screen ()
  "A preview arrives with line endings and blank rows at its foot, and what is
kept is the picture, held to the rows the screen has for it."
  (let ((lines (veron::preview-lines
                (format nil "AAA~C~%BBB~C~%~C~%~C~%" #\Return #\Return
                        #\Return #\Return))))
    (assert (equal '("AAA" "BBB") lines) () "Kept ~S" lines))
  (let ((many (veron::preview-lines
               (with-output-to-string (out)
                 (dotimes (row 30) (format out "Zeile ~D~%" row))))))
    (assert (= veron::*preview-rows* (length many)) ()
            "~D rows kept, the screen has ~D" (length many) veron::*preview-rows*))
  (let ((margin (floor (- 80 veron::*preview-columns*) 2)))
    (assert (string= (concatenate 'string (make-string margin :initial-element #\Space) "x")
                     (first (veron::preview-display-lines '("x"))))
            () "The picture should be set in from the left")))

(define-test print-failure-reason-is-what-went-wrong ()
  "A socket error opens with the stream it happened on, so the reason the run
reports is the line below it."
  (assert (string= "Connection reset by peer"
                   (veron::failure-reason
                    (make-condition
                     'simple-error
                     :format-control
                     "Couldn't write to #<FD-STREAM for \"socket 1.2.3.4:3272\">:~%  Connection reset by peer")))
          () "The reason should read on its own")
  (assert (string= "kaputt"
                   (veron::failure-reason
                    (make-condition 'simple-error :format-control "kaputt")))
          () "A single-line condition is its own reason"))

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
              () "Should not ask for a photo id when there is no printer")
      (assert-cursor-at s 4 3
                        :description "Cursor should wait on the line that says why"))))

(define-test e2e-fotodruck-wait-updates-itself ()
  "A visitor kept waiting sees the form arrive when a printer does, without
touching the keyboard."
  (with-print-listener (port)
    (let ((printer (veron::find-printer "nec-p6"))
          (relay nil))
      (unwind-protect
           (with-veron-app (s :username "printuser9" :password "printpass9")
             (login s "printuser9" "printpass9")
             (select-menu-item s "Fotodruck")
             (assert (wait-for-screen-contains s "kein Drucker verbunden" :timeout 3)
                     () "Should start out with nothing to print on")
             ;; The relay dials in while the visitor sits on the waiting page.
             (setf relay (connect-relay port "nec-p6 test-token"))
             (assert (wait-until (lambda () (veron::printer-ready-p printer))) ()
                     "Printer should become ready")
             (assert (wait-for-screen-contains s "Foto-ID" :timeout 5)
                     () "The form should arrive on its own once there is a printer")
             (assert-cursor-at s 8 14
                               :description "Cursor should be on the id field"))
        (when relay (ignore-errors (usocket:socket-close relay)))))))

(defun test-print-run (length)
  (let ((data (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length data)
      (setf (aref data i) (mod (* i 7) 256)))))

(defun test-preview-text ()
  "Sixteen rows of a preview as the website keeps them, a letter to a row."
  (with-output-to-string (out)
    (dotimes (row veron::*preview-rows*)
      (format out "~A~C~C"
              (make-string veron::*preview-columns*
                           :initial-element (code-char (+ (char-code #\A) row)))
              #\Return #\Newline))))

(defun preview-website (photo-id &optional runs)
  "A website that has the preview for PHOTO-ID, and the runs RUNS names as
(path . octets)."
  (lambda (path)
    (cond ((string= path (format nil "/foto/~A/ascii-veron.txt" photo-id))
           (values 200 (babel:string-to-octets (test-preview-text))))
          ((assoc path runs :test #'string=)
           (values 200 (cdr (assoc path runs :test #'string=))))
          (t (values 404 (babel:string-to-octets "no"))))))

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
      (with-photo-website ((preview-website
                            "K7NP4M"
                            (list (cons "/foto/K7NP4M/nec-p6-180.prn" run))))
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
                 (press-enter s)
                 (assert-on-screen s "FOTODRUCK-VORSCHAU")
                 (assert (wait-for-screen-contains s "Auflösung" :timeout 3)
                         () "The preview should ask for the density")
                 (move-cursor s 18 13)
                 (type-text s "2")
                 (press-enter s)
                 (assert-on-screen s "FOTODRUCK-STATUS")
                 (assert-cursor-at s 2 3
                                   :description "Cursor should head the report")
                 (let ((received (read-until-eof relay)))
                   (assert (= (length run) (length received)) ()
                           "Expected ~D bytes at the relay, got ~D"
                           (length run) (length received))
                   (assert (every #'= run received) () "The run arrived altered"))
                 (assert (wait-for-screen-contains s "Der Ausdruck ist fertig" :timeout 5)
                         () "The status screen should report the sheet as finished")
                 (let ((full (format nil "~{~A~^~%~}" (screen-text s))))
                   (assert (search "NEC Pinwriter P6" full) () "Should name the printer")
                   (assert (search "180 dpi" full) () "Should name the density"))
                 ;; The form is done with once the sheet is on its way, so
                 ;; leaving the report reaches the menu rather than the form.
                 (press-pf s 3)
                 (assert-on-screen s "MAIN")))
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
                 (press-enter s)
                 (assert-on-screen s "FOTODRUCK")
                 (assert (wait-for-screen-contains s "unbekannt" :timeout 3)
                         () "Should say the photo id is unknown")
                 (assert-cursor-at s 8 14
                                   :description "Cursor should return to the id field")
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
                 (press-enter s)
                 (assert (wait-for-screen-contains s "aufbereitet" :timeout 3)
                         () "Should say the photo is still being converted")
                 (assert-cursor-at s 8 14
                                   :description "Cursor should return to the id field")))
          (ignore-errors (usocket:socket-close relay)))))))

(define-test e2e-fotodruck-id-in-capitals ()
  "An id typed in lower case is taken up in capitals, which is the shape the
website is asked for it in, and the preview follows on it."
  (with-print-listener (port)
    (let ((printer (veron::find-printer "nec-p6"))
          (relay (connect-relay port "nec-p6 test-token"))
          (asked nil))
      (with-photo-website ((lambda (path)
                             (push path asked)
                             (funcall (preview-website "K7NP4M") path)))
        (unwind-protect
             (progn
               (assert (wait-until (lambda () (veron::printer-ready-p printer))) ()
                       "Printer should be ready")
               (with-veron-app (s :username "printuser7" :password "printpass7")
                 (login s "printuser7" "printpass7")
                 (select-menu-item s "Fotodruck")
                 (assert (wait-for-screen-contains s "Foto-ID" :timeout 3)
                         () "Should ask for the photo id")
                 (move-cursor s 8 14)
                 (type-text s "k7np4m")
                 (press-enter s)
                 (assert-on-screen s "FOTODRUCK-VORSCHAU")
                 (assert (member "/foto/K7NP4M/ascii-veron.txt" asked :test #'string=)
                         () "The website should be asked in capitals, was asked ~S" asked)
                 (assert-cursor-at s 18 13
                                   :description "Cursor should be on the density field")))
          (ignore-errors (usocket:socket-close relay)))))))

(define-test e2e-fotodruck-malformed-id ()
  "An id that is not one is answered in the id field, not somewhere else."
  (with-print-listener (port)
    (let ((printer (veron::find-printer "nec-p6"))
          (relay (connect-relay port "nec-p6 test-token")))
      (unwind-protect
           (progn
             (assert (wait-until (lambda () (veron::printer-ready-p printer))) ()
                     "Printer should be ready")
             (with-veron-app (s :username "printuser10" :password "printpass10")
               (login s "printuser10" "printpass10")
               (select-menu-item s "Fotodruck")
               (assert (wait-for-screen-contains s "Foto-ID" :timeout 3)
                       () "Should ask for the photo id")
               ;; O is not in the booth's alphabet, so this is not an id.
               (move-cursor s 8 14)
               (type-text s "K7NPO4")
               (press-enter s)
               (assert-message s "sechsstellige Foto-ID")
               (assert-cursor-at s 8 14
                                 :description "Cursor should be on the id field")))
        (ignore-errors (usocket:socket-close relay))))))

(define-test e2e-fotodruck-busy ()
  "A printer with a sheet to finish says it is busy rather than reporting itself gone."
  (with-print-listener (port)
    (let* ((printer (veron::find-printer "nec-p6"))
           (resolution (veron::find-resolution printer 180))
           (relay (connect-relay port "nec-p6 test-token")))
      (unwind-protect
           (progn
             (assert (wait-until (lambda () (veron::printer-ready-p printer))) ()
                     "Printer should be ready")
             ;; Claim the printer without sending, so the sheet stays on its way.
             (let ((job (make-instance 'veron::print-job
                                       :printer printer :resolution resolution
                                       :username "otheruser" :data nil :total 1000)))
               (setf (veron::print-job-state job) :running
                     (veron::print-job-sent job) 500
                     (veron::printer-job printer) job))
             (with-veron-app (s :username "printuser8" :password "printpass8")
               (login s "printuser8" "printpass8")
               (select-menu-item s "Fotodruck")
               (assert-on-screen s "FOTODRUCK")
               (assert (wait-for-screen-contains s "belegt" :timeout 3)
                       () "Should say the printer is busy")
               ;; The run has only just started, so how long it still needs is
               ;; being measured rather than claimed.
               (assert (wait-for-screen-contains s "gemessen" :timeout 3)
                       () "Should say the remaining time is being measured")
               (assert (not (search "kein Drucker verbunden"
                                    (format nil "~{~A~^~%~}" (screen-text s))))
                       () "A busy printer is not a printer that is gone")))
        (ignore-errors (usocket:socket-close relay))))))

(define-test e2e-fotodruck-offers-the-form ()
  "With a relay connected the form asks for an id and names the printer."
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
                 (assert (search "NEC Pinwriter P6" full) ()
                         "Should name the connected printer")
                 (assert (search "Foto-ID" full) ()
                         "Should label the id field"))
               (assert-cursor-at s 8 14
                                 :description "Cursor should be on the id field")))
        (ignore-errors (usocket:socket-close socket))))))

(define-test e2e-fotodruck-preview-offers-the-densities ()
  "The preview stands where the picture goes, with the densities on the one line
under it, and going back reaches the form the id was typed into."
  (with-print-listener (port)
    (let ((printer (veron::find-printer "nec-p6"))
          (relay (connect-relay port "nec-p6 test-token")))
      (with-photo-website ((preview-website "K7NP4M"))
        (unwind-protect
             (progn
               (assert (wait-until (lambda () (veron::printer-ready-p printer))) ()
                       "Printer should be ready")
               (with-veron-app (s :username "printuser11" :password "printpass11")
                 (login s "printuser11" "printpass11")
                 (select-menu-item s "Fotodruck")
                 (assert (wait-for-screen-contains s "Foto-ID" :timeout 3)
                         () "Should ask for the photo id")
                 (move-cursor s 8 14)
                 (type-text s "K7NP4M")
                 (press-enter s)
                 (assert-on-screen s "FOTODRUCK-VORSCHAU")
                 ;; The picture keeps the rows above the density line, set in
                 ;; from the left so it stands in the middle of the screen.
                 (assert-text-at s 1 13 veron::*preview-columns*
                                 (make-string veron::*preview-columns*
                                              :initial-element #\A)
                                 :description "The first row of the preview")
                 (assert-text-at s 16 13 veron::*preview-columns*
                                 (make-string veron::*preview-columns*
                                              :initial-element #\P)
                                 :description "The last row of the preview")
                 (let ((line (screen-text-at s 18 0 79)))
                   (dolist (label '("Auflösung" "Niedrig" "Mittel" "Hoch"))
                     (assert (search label line) ()
                             "~A should be on the density line, which reads ~S"
                             label line)))
                 (press-pf s 3)
                 (assert-on-screen s "FOTODRUCK")
                 (assert (wait-for-screen-contains s "Foto-ID" :timeout 3)
                         () "Going back should reach the form")))
          (ignore-errors (usocket:socket-close relay)))))))
