;;; -*- Mode: Lisp -*-

;;; Fotodruck: a visitor's photo on the exhibition's dot-matrix printer.
;;;
;;; The printer hangs off a print server on the show's internal network, which
;;; has no inbound route, so the relay beside it dials in here and holds the
;;; connection open. A connection present is what "the printer is there" means;
;;; the bytes of a print run go down it, and closing it is how the relay is told
;;; the run has ended.
;;;
;;; The run itself is fetched from the exhibition website, which keeps a .prn
;;; per photo and density, made by fotofix. Nothing is rendered here.

(in-package #:veron)

;;; Configuration

(defclass print-resolution ()
  ((dpi :initarg :dpi :reader print-resolution-dpi)
   (label :initarg :label :reader print-resolution-label)
   (minutes :initarg :minutes :reader print-resolution-minutes))
  (:documentation "One density a printer offers, and how long a sheet takes at it."))

(defclass printer ()
  ((name :initarg :name :reader printer-name)
   (stem :initarg :stem :reader printer-stem
         :documentation "File-name stem on the website, and the name the relay signs on with.")
   (resolutions :initarg :resolutions :reader printer-resolutions)
   (lock :initform (bt:make-lock "printer") :reader printer-lock)
   (relay :initform nil :accessor printer-relay
          :documentation "The socket the relay is connected on, or NIL.")
   (job :initform nil :accessor printer-job
        :documentation "The most recent job, running or finished."))
  (:documentation "A printer at the show and the relay that feeds it."))

(defparameter *printers*
  (list (make-instance
         'printer
         :name "NEC Pinwriter P6"
         :stem "nec-p6"
         ;; TODO: measured at the show's printer
         :resolutions (list (make-instance 'print-resolution :dpi 360 :label "Hoch" :minutes 99)
                            (make-instance 'print-resolution :dpi 180 :label "Mittel" :minutes 99)
                            (make-instance 'print-resolution :dpi 60 :label "Niedrig" :minutes 99))))
  "The printers the show can offer. A new one is another entry here.")

(defun print-listener-port ()
  "The port the relay dials, or NIL when Fotodruck is not configured."
  (when-let (value (env "VERON_PRINT_PORT" nil))
    (parse-integer value :junk-allowed t)))

(defun print-token ()
  "The secret the relay signs on with."
  (env "VERON_PRINT_TOKEN" ""))

(defun fotofix-base-url ()
  "Where the print runs are fetched from."
  (string-right-trim "/" (env "VERON_FOTOFIX_URL" "https://2026.classic-computing.de")))

(defun fotodruck-configured-p ()
  "Return T when a listener port and a token are set."
  (and (print-listener-port) (plusp (length (print-token)))))

(defun find-printer (stem)
  (find stem *printers* :key #'printer-stem :test #'string=))

(defun find-resolution (printer dpi)
  (find dpi (printer-resolutions printer) :key #'print-resolution-dpi))

;;; Jobs

(defclass print-job ()
  ((printer :initarg :printer :reader print-job-printer)
   (resolution :initarg :resolution :reader print-job-resolution)
   (username :initarg :username :reader print-job-username)
   (data :initarg :data :accessor print-job-data
         :documentation "The run itself, dropped once it has gone out.")
   (total :initarg :total :reader print-job-total)
   (sent :initform 0 :accessor print-job-sent)
   (started-at :initform (get-universal-time) :reader print-job-started-at)
   (state :initform :queued :accessor print-job-state)
   (message :initform nil :accessor print-job-message))
  (:documentation "One print run on its way to a printer."))

(defun job-active-p (job)
  "Return T while JOB is waiting to go out or going out."
  (and job (member (print-job-state job) '(:queued :running))))

(defun printer-busy-p (printer)
  (job-active-p (printer-job printer)))

(defun printer-ready-p (printer)
  "Return T when PRINTER has a relay connected and is not printing."
  (and (printer-relay printer) (not (printer-busy-p printer))))

(defun ready-printers ()
  (remove-if-not #'printer-ready-p *printers*))

(defun connected-printers ()
  (remove-if-not #'printer-relay *printers*))

(defun print-job-fraction (job)
  "How much of the run has gone out, from 0 to 1."
  (let ((total (print-job-total job)))
    (if (plusp total)
        (/ (print-job-sent job) total)
        1)))

(defun print-job-remaining-minutes (job)
  (round (* (print-resolution-minutes (print-job-resolution job))
            (- 1 (print-job-fraction job)))))

;;; The relay connection
;;;
;;; One line signs on — the printer's stem and the token, separated by a space —
;;; and everything after it travels the other way.

(defvar *print-listener-socket* nil)

(defvar *print-listener-thread* nil)

(defun read-relay-line (socket timeout limit)
  "Read one newline-terminated ASCII line from SOCKET.
Returns NIL when the line does not arrive within TIMEOUT seconds, exceeds
LIMIT bytes, or the relay disconnects first."
  (let ((stream (usocket:socket-stream socket))
        (deadline (+ (get-universal-time) timeout))
        (bytes (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer t)))
    (loop
      (when (> (get-universal-time) deadline)
        (return nil))
      (when (or (listen stream)
                (usocket:wait-for-input socket :timeout 1 :ready-only t))
        (let ((byte (read-byte stream nil nil)))
          (unless byte
            (return nil))
          (when (= byte 10)
            (return (babel:octets-to-string (coerce bytes '(vector (unsigned-byte 8)))
                                            :encoding :ascii :errorp nil)))
          (unless (= byte 13)
            (vector-push-extend byte bytes))
          (when (> (fill-pointer bytes) limit)
            (return nil)))))))

(defun attach-relay (printer socket)
  "Make SOCKET the relay for PRINTER. Returns NIL while a job is running.
A relay that reconnects while an older socket is still registered replaces it,
so a connection dropped without a close does not keep the printer occupied."
  (bt:with-lock-held ((printer-lock printer))
    (unless (job-active-p (printer-job printer))
      (when-let (previous (printer-relay printer))
        (ignore-errors (usocket:socket-close previous)))
      (setf (printer-relay printer) socket))))

(defun detach-relay (printer socket)
  "Forget SOCKET as PRINTER's relay, unless a newer one has taken its place."
  (bt:with-lock-held ((printer-lock printer))
    (when (eq (printer-relay printer) socket)
      (setf (printer-relay printer) nil))))

(defun serve-relay (printer socket)
  "Hold the connection until a run is ready for it, or the relay goes away.
The relay says nothing after signing on, so anything readable is the connection
closing. Sending happens here rather than in the thread that took the order, so
one thread owns the socket for its whole life: the close that ends a run is then
this thread's own, and reaches the relay as a close rather than as a socket
shut from under a blocked reader."
  (loop
    (when (usocket:wait-for-input socket :timeout 0.2 :ready-only t)
      (return))
    (let ((job (printer-job printer)))
      (when (and job (eq (print-job-state job) :queued))
        (send-print-run job socket)
        (return)))))

(defun handle-relay-connection (socket)
  (let* ((line (read-relay-line socket 30 200))
         (space (and line (position #\Space line)))
         (stem (and space (subseq line 0 space)))
         (token (and space (string-trim " " (subseq line (1+ space)))))
         (printer (and stem (find-printer stem))))
    (unless (and printer
                 (plusp (length (print-token)))
                 (string= token (print-token)))
      (lispf:log-message :warn "fotodruck: Anmeldung von ~A abgewiesen"
                         (ignore-errors (usocket:get-peer-address socket)))
      (return-from handle-relay-connection))
    (unless (attach-relay printer socket)
      (lispf:log-message :warn "fotodruck: ~A druckt gerade, Verbindung abgewiesen"
                         (printer-name printer))
      (return-from handle-relay-connection))
    (lispf:log-message :info "fotodruck: ~A verbunden" (printer-name printer))
    (unwind-protect
         (serve-relay printer socket)
      (detach-relay printer socket)
      (lispf:log-message :info "fotodruck: ~A getrennt" (printer-name printer)))))

(defun accept-relay-connections (listener)
  (loop
    (let ((socket (usocket:socket-accept listener)))
      (bt:make-thread
       (lambda ()
         (unwind-protect
              (handler-case (handle-relay-connection socket)
                (error (e)
                  (lispf:log-message :warn "fotodruck: Relaisfehler: ~A" e)))
           (ignore-errors (usocket:socket-close socket))))
       :name "fotodruck-relay"))))

(defun start-print-listener (&key (host "0.0.0.0"))
  "Start listening for the print relay, if Fotodruck is configured.
The port is bound here rather than in the accepting thread, so a relay that
connects the moment this returns finds the port open."
  (when (and (fotodruck-configured-p) (not *print-listener-thread*))
    (let* ((port (print-listener-port))
           (listener (usocket:socket-listen host port
                                            :element-type '(unsigned-byte 8)
                                            :reuse-address t)))
      (setf *print-listener-socket* listener)
      (lispf:log-message :info "fotodruck: Druckeranschluss auf ~A:~D" host port)
      (setf *print-listener-thread*
            (bt:make-thread
             (lambda ()
               (unwind-protect
                    (handler-case (accept-relay-connections listener)
                      (error (e)
                        (lispf:log-message :info "fotodruck: Druckeranschluss beendet: ~A" e)))
                 (ignore-errors (usocket:socket-close listener))))
             :name "fotodruck-listener")))))

(defun stop-print-listener ()
  (when *print-listener-socket*
    (ignore-errors (usocket:socket-close *print-listener-socket*))
    (setf *print-listener-socket* nil))
  (when *print-listener-thread*
    (ignore-errors (bt:destroy-thread *print-listener-thread*))
    (setf *print-listener-thread* nil)))

;;; Sending a run

(defparameter *print-chunk-size* 4096
  "Written and flushed a chunk at a time, so the printer's own pace shows up
as progress rather than as one long write that returns when the last byte is
buffered somewhere.")

(defun finish-print-job (job state message)
  (setf (print-job-state job) state
        (print-job-message job) message)
  (lispf:log-message (if (eq state :done) :info :warn)
                     "fotodruck: ~A ~Ddpi ~A~@[ (~A)~] nach ~A"
                     (print-job-username job)
                     (print-resolution-dpi (print-job-resolution job))
                     (if (eq state :done) "fertig" "abgebrochen")
                     message
                     (format-duration (- (get-universal-time) (print-job-started-at job)))))

(defun send-print-run (job socket)
  "Write the run to SOCKET. The caller closes it, which ends the run."
  (let ((data (print-job-data job)))
    (setf (print-job-state job) :running)
    (handler-case
        (let ((stream (usocket:socket-stream socket))
              (total (length data)))
          (loop for start from 0 by *print-chunk-size* below total
                for end = (min total (+ start *print-chunk-size*))
                do (write-sequence data stream :start start :end end)
                   (force-output stream)
                   (setf (print-job-sent job) end))
          (finish-print-job job :done nil))
      (error (e)
        (finish-print-job job :failed (princ-to-string e))))
    (setf (print-job-data job) nil)))

(defun start-print-job (printer resolution username data)
  "Hand DATA to PRINTER for its relay to send.
Returns the job, or NIL when the printer has meanwhile become unavailable."
  (when-let (job (bt:with-lock-held ((printer-lock printer))
                   (when (and (printer-relay printer)
                              (not (job-active-p (printer-job printer))))
                     (setf (printer-job printer)
                           (make-instance 'print-job :printer printer
                                                     :resolution resolution
                                                     :username username
                                                     :data data
                                                     :total (length data))))))
    (lispf:log-message :info "fotodruck: ~A ~Ddpi ~D Bytes an ~A"
                       username (print-resolution-dpi resolution)
                       (length data) (printer-name printer))
    job))

;;; The website

(defparameter *photo-id-alphabet* "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"
  "The characters a photo id is drawn from: no I and no O, so nothing has to be
read as a one or a zero.")

(defun valid-photo-id-p (id)
  (and (= (length id) 6)
       (every (lambda (char) (find char *photo-id-alphabet*)) id)))

(defun http-get-octets (url)
  "GET URL. Returns the body as octets and the status, or NIL and the status."
  (handler-case
      (dex:get url :force-binary t :connect-timeout 10 :read-timeout 120)
    (dex:http-request-failed (e)
      (values nil (dex:response-status e)))
    (error (e)
      (lispf:log-message :warn "fotodruck: ~A nicht erreichbar: ~A" url e)
      (values nil nil))))

(defun photo-state (photo-id)
  "Ask the website about PHOTO-ID: :unknown, :deleted, :converting or :ready."
  (multiple-value-bind (body status)
      (http-get-octets (format nil "~A/api/visitor-photo/~A/page"
                               (fotofix-base-url) photo-id))
    (if (and body (eql status 200))
        (let ((json (yason:parse (babel:octets-to-string body :encoding :utf-8))))
          (cond ((gethash "deleted" json) :deleted)
                ((gethash "converting" json) :converting)
                (t :ready)))
        :unknown)))

(defun fetch-print-run (printer resolution photo-id)
  "Fetch the print run for PHOTO-ID at RESOLUTION.
Returns the octets, or NIL and a keyword saying why there are none:
:unknown, :deleted, :converting, :missing or :unreachable."
  (multiple-value-bind (body status)
      (http-get-octets (format nil "~A/foto/~A/datei/~A-~D.prn"
                               (fotofix-base-url) photo-id
                               (printer-stem printer)
                               (print-resolution-dpi resolution)))
    (cond ((and body (eql status 200))
           body)
          ((null status)
           (values nil :unreachable))
          (t
           (values nil (case (photo-state photo-id)
                         (:unknown :unknown)
                         (:deleted :deleted)
                         (:converting :converting)
                         (t :missing)))))))
