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
   (seconds :initarg :seconds :reader print-resolution-seconds))
  (:documentation "One density a printer offers, and how long a sheet takes at it."))

(defclass printer ()
  ((name :initarg :name :accessor printer-name)
   (stem :initarg :stem :reader printer-stem
         :documentation "File-name stem on the website, and the name the relay signs on with.")
   (resolutions :initarg :resolutions :accessor printer-resolutions)
   (lock :initform (bt:make-lock "printer") :reader printer-lock)
   (relay :initform nil :accessor printer-relay
          :documentation "The socket the relay is connected on, or NIL.")
   (job :initform nil :accessor printer-job
        :documentation "The most recent job, running or finished."))
  (:documentation "A printer at the show and the relay that feeds it."))

(defvar *printers* nil
  "The printers the show can offer, as live objects: a hot reload updates what
each one is called and what it offers while leaving the relay connected to it
in place.")

(defun find-printer (stem)
  (find stem *printers* :key #'printer-stem :test #'string=))

(defun register-printer (stem name resolutions)
  "Define the printer known by STEM, or update the one already there."
  (if-let (printer (find-printer stem))
    (setf (printer-name printer) name
          (printer-resolutions printer) resolutions)
    (setf *printers* (append *printers*
                             (list (make-instance 'printer :name name :stem stem
                                                           :resolutions resolutions)))))
  stem)

(defun resolution (dpi label seconds)
  (make-instance 'print-resolution :dpi dpi :label label :seconds seconds))

(defun register-show-printers ()
  "The printers at the show, coarsest density first. A new one is another call here."
  (register-printer "nec-p6" "NEC Pinwriter P6"
                    (list (resolution 60 "Niedrig" 60)
                          (resolution 180 "Mittel" 210)
                          (resolution 360 "Hoch" 300))))

(register-show-printers)

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
   (finished-at :initform nil :accessor print-job-finished-at)
   (state :initform :queued :accessor print-job-state)
   (message :initform nil :accessor print-job-message))
  (:documentation "One print run on its way to a printer."))

(defun job-active-p (job)
  "Return T while JOB is waiting to go out, going out, or reaching the paper."
  (and job (member (print-job-state job) '(:queued :running :draining))))

(defparameter *relay-return-seconds* 30
  "How long a printer whose run has just ended is given for its relay to dial
back in before it counts as gone.")

(defun printer-busy-p (printer)
  "Return T while PRINTER has a sheet to finish.
The relay's connection ends with the run and is dialled again, so a printer
whose relay has just gone is one that has just printed rather than one that has
gone away."
  (when-let (job (printer-job printer))
    (or (job-active-p job)
        (and (null (printer-relay printer))
             (print-job-finished-at job)
             (< (- (get-universal-time) (print-job-finished-at job))
                *relay-return-seconds*)
             t))))

(defun printer-ready-p (printer)
  "Return T when PRINTER has a relay connected and is not printing."
  (and (printer-relay printer) (not (printer-busy-p printer))))

(defun ready-printers ()
  (remove-if-not #'printer-ready-p *printers*))

(defun print-job-fraction (job)
  "How much of the run has gone out, from 0 to 1."
  (let ((total (print-job-total job)))
    (if (plusp total)
        (/ (print-job-sent job) total)
        1)))

(defun print-job-seconds (job)
  "How long the sheet takes at the density it was ordered in."
  (print-resolution-seconds (print-job-resolution job)))

(defparameter *print-drain-seconds* 10
  "How much longer the printer works after the last byte of a run has gone out.
The bytes on their way from here to the paper are worth about this much, so the
sheet is not finished when the writing is.")

(defparameter *estimate-warmup-seconds* 15
  "How long a run goes before what has gone out says anything about its pace.")

(defparameter *estimate-step-seconds* 15
  "Estimates are rounded up to this, so that they do not read as more exact than
they are.")

(defun print-job-remaining-seconds (job)
  "How much longer the sheet takes, or NIL while that is still being measured.
A printer that holds the run back stretches what is left along with it, so the
figure follows the sheet rather than the schedule it was started on, and the
bytes still on their way to the paper are counted in."
  (let ((elapsed (- (get-universal-time) (print-job-started-at job))))
    (when (< elapsed *estimate-warmup-seconds*)
      (return-from print-job-remaining-seconds nil))
    (let* ((fraction (print-job-fraction job))
           (expected (print-job-seconds job))
           (projected (if (plusp fraction) (/ elapsed fraction) expected))
           (left (+ (* (- 1 fraction) (max expected projected))
                    *print-drain-seconds*)))
      (* *estimate-step-seconds*
         (max 1 (ceiling left *estimate-step-seconds*))))))

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

(defparameter *keepalive-idle-seconds* 60)

(defparameter *keepalive-interval-seconds* 10)

(defparameter *keepalive-probes* 3)

(defun keep-alive (socket)
  "Ask the kernel to probe the connection while it is idle.
A relay holds its socket open for hours with nothing to say on it, so a relay
that goes away without a close would otherwise leave the printer looking
connected for as long as the process lives. The probes settle it in about a
minute and a half."
  (let ((raw (usocket:socket socket)))
    (setf (sb-bsd-sockets:sockopt-keep-alive raw) t)
    ;; When the probing starts and how often it repeats is a knob the platform
    ;; may not offer; where it does not, its own timing stands.
    (ignore-errors
     (setf (sb-bsd-sockets:sockopt-tcp-keepidle raw) *keepalive-idle-seconds*))
    (ignore-errors
     (setf (sb-bsd-sockets:sockopt-tcp-keepintvl raw) *keepalive-interval-seconds*))
    (ignore-errors
     (setf (sb-bsd-sockets:sockopt-tcp-keepcnt raw) *keepalive-probes*))))

(defparameter *print-send-buffer-bytes* 8192
  "How much of a run the kernel may hold for the relay. A short queue is what
makes a printer slower than its schedule hold the writes back, instead of the
run disappearing into a buffer and looking finished.")

(defun limit-send-buffer (socket)
  "Keep the kernel's queue for SOCKET short, so the printer's pace reaches here."
  (ignore-errors
   (setf (sb-bsd-sockets:sockopt-send-buffer (usocket:socket socket))
         *print-send-buffer-bytes*)))

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
                         (ignore-errors
                           (usocket:host-to-hostname
                            (usocket:get-peer-address socket))))
      (return-from handle-relay-connection))
    ;; Set up the socket before it is registered, so that it is a fully
    ;; configured connection by the time anything can find it.
    (keep-alive socket)
    (limit-send-buffer socket)
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

(defparameter *print-chunk-size* 1024
  "Written and flushed a chunk at a time, so what has gone out is what the
printer is working on rather than what a buffer swallowed.")

(defparameter *print-tick-seconds* 0.5
  "How often a run that is ahead of the sheet looks at the clock again.")

(defun relay-address (socket)
  "SOCKET's far end as text, so a report names the relay a run went to."
  (or (ignore-errors
        (format nil "~A:~D"
                (usocket:host-to-hostname (usocket:get-peer-address socket))
                (usocket:get-peer-port socket)))
      "unbekannt"))

(defun failure-reason (condition)
  "The last line of CONDITION's text, which is what went wrong.
A socket error opens with the stream it happened on, and that stream is veron's
own end of the relay connection: the reason itself is on the line below it."
  (let* ((text (princ-to-string condition))
         (newline (position #\Newline text :from-end t)))
    (string-trim '(#\Space #\Tab)
                 (if newline (subseq text (1+ newline)) text))))

(defun finish-print-job (job state message &optional detail)
  "Record how JOB ended. MESSAGE is what the visitor reads on the report, DETAIL
what the log adds about where the run stopped."
  (setf (print-job-state job) state
        (print-job-message job) message
        (print-job-finished-at job) (get-universal-time))
  (lispf:log-message (if (eq state :done) :info :warn)
                     "fotodruck: ~A ~Ddpi ~A~@[ (~A)~] nach ~A"
                     (print-job-username job)
                     (print-resolution-dpi (print-job-resolution job))
                     (if (eq state :done) "fertig" "abgebrochen")
                     detail
                     (format-duration (- (get-universal-time) (print-job-started-at job)))))

(defun paced-limit (job total elapsed)
  "How many of TOTAL bytes may have gone out after ELAPSED seconds."
  (let ((seconds (print-job-seconds job)))
    (if (plusp seconds)
        (min total (floor (* total elapsed) seconds))
        total)))

(defun send-print-run (job socket)
  "Write the run to SOCKET at the sheet's own pace. The caller closes it, which
ends the run.
The printer takes minutes over a sheet that the network carries in a moment, so
the bytes go out on the schedule the density is printed at. What is left in the
buffers between here and the paper then stays small, which makes the run end
when the sheet does and the bytes gone out a fair measure of the sheet's
progress. A printer slower than the schedule holds the writes back, and the
figures follow it.
The last bytes are still on their way to the paper when the writing is over, so
the run waits out that much before it counts as finished."
  (let* ((data (print-job-data job))
         (total (length data))
         (started (get-internal-real-time)))
    (setf (print-job-state job) :running)
    (handler-case
        (let ((stream (usocket:socket-stream socket)))
          (loop for sent = (print-job-sent job)
                while (< sent total)
                for elapsed = (/ (- (get-internal-real-time) started)
                                 internal-time-units-per-second)
                for limit = (paced-limit job total elapsed)
                do (if (and (< limit total) (< (- limit sent) *print-chunk-size*))
                       (sleep *print-tick-seconds*)
                       (progn
                         (write-sequence data stream :start sent :end limit)
                         (force-output stream)
                         (setf (print-job-sent job) limit))))
          (setf (print-job-state job) :draining)
          (sleep *print-drain-seconds*)
          (finish-print-job job :done nil))
      (error (e)
        (finish-print-job job :failed
                          "Die Verbindung zum Drucker ist abgerissen."
                          (format nil "~A über Relais ~A: ~A"
                                  (printer-name (print-job-printer job))
                                  (relay-address socket)
                                  (failure-reason e)))))
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

(defparameter *preview-file* "ascii-veron.txt"
  "What the website calls the preview a visitor is shown before they print.")

(defparameter *preview-columns* 53
  "How wide the preview is drawn, which is what the screen centres it on.")

(defparameter *preview-rows* 16
  "How many rows of the screen the preview stands in.")

(defun no-file-reason (photo-id)
  "Why the website has no such file for PHOTO-ID: :unknown, :deleted or
:converting when the photo itself is the reason, and :missing when the photo is
there and this file is not."
  (case (photo-state photo-id)
    (:unknown :unknown)
    (:deleted :deleted)
    (:converting :converting)
    (t :missing)))

(defun preview-lines (text)
  "TEXT as the rows of a preview: line endings off, blank rows at the foot
dropped, and no more rows than there is screen to stand them in."
  (let ((lines (mapcar (lambda (line) (string-right-trim '(#\Return #\Space) line))
                       (uiop:split-string text :separator (list #\Newline)))))
    (loop while (and lines (string= "" (first (last lines))))
          do (setf lines (butlast lines)))
    (subseq lines 0 (min (length lines) *preview-rows*))))

(defun fetch-preview (photo-id)
  "Fetch the preview for PHOTO-ID.
Returns its rows, or NIL and a keyword saying why there are none:
:unknown, :deleted, :converting, :missing or :unreachable."
  (multiple-value-bind (body status)
      (http-get-octets (format nil "~A/foto/~A/~A"
                               (fotofix-base-url) photo-id *preview-file*))
    (cond ((and body (eql status 200))
           (preview-lines (babel:octets-to-string body :encoding :latin-1)))
          ((null status)
           (values nil :unreachable))
          (t
           (values nil (no-file-reason photo-id))))))

(defun fetch-print-run (printer resolution photo-id)
  "Fetch the print run for PHOTO-ID at RESOLUTION.
Returns the octets, or NIL and a keyword saying why there are none:
:unknown, :deleted, :converting, :missing or :unreachable."
  (multiple-value-bind (body status)
      (http-get-octets (format nil "~A/foto/~A/~A-~D.prn"
                               (fotofix-base-url) photo-id
                               (printer-stem printer)
                               (print-resolution-dpi resolution)))
    (cond ((and body (eql status 200))
           body)
          ((null status)
           (values nil :unreachable))
          (t
           (values nil (no-file-reason photo-id))))))
