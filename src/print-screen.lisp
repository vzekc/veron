;;; -*- Mode: Lisp -*-

;;; Fotodruck screens: the form a visitor types their photo id into, and the
;;; live view of the sheet coming out.

(in-package #:veron)

(defun print-duration-text (seconds)
  "SECONDS in the words these screens use: whole minutes by name, the rest as m:ss."
  (multiple-value-bind (minutes rest) (floor (round seconds) 60)
    (cond ((and (zerop minutes) (= rest 1)) "1 Sekunde")
          ((zerop minutes) (format nil "~D Sekunden" rest))
          ((and (= minutes 1) (zerop rest)) "1 Minute")
          ((zerop rest) (format nil "~D Minuten" minutes))
          (t (format nil "~D:~2,'0D Minuten" minutes rest)))))

(defun printer-choice-line (items index)
  "The INDEX-th (1-based) printer as a choice line, or an empty line."
  (if-let (printer (nth (1- index) items))
    (format nil "~D  ~A" index (printer-name printer))
    ""))

(defun resolution-choice-line (items index)
  "The INDEX-th (1-based) resolution as a choice line, or an empty line."
  (if-let (resolution (nth (1- index) items))
    (format nil "~D  ~8A (~3D dpi)  ca. ~A"
            index
            (print-resolution-label resolution)
            (print-resolution-dpi resolution)
            (print-duration-text (print-resolution-seconds resolution)))
    ""))

(defun resolution-choice-text (items)
  "The densities on one line, as the preview asks for them: the picture has the
screen, so what is left for the choice is the row under it."
  (format nil "~{~A~^   ~}"
          (loop for resolution in items
                for index from 1
                collect (format nil "~D ~A, ~A"
                                index
                                (print-resolution-label resolution)
                                (print-duration-text
                                 (print-resolution-seconds resolution))))))

(defun selection-index (value)
  "The 1-based number typed into a selection field, or NIL."
  (let ((digits (string-trim " " value)))
    (when (and (= (length digits) 1) (digit-char-p (char digits 0)))
      (digit-char-p (char digits 0)))))

(defun selected-item (items value)
  "The item VALUE selects, or the only item when there is just one."
  (cond ((null items) nil)
        ((null (rest items))
         (let ((index (selection-index value)))
           (if (and index (/= index 1)) nil (first items))))
        (t (when-let (index (selection-index value))
             (nth (1- index) items)))))

(defun busy-printer-note (printer)
  "What to say while PRINTER still has a sheet to finish."
  (let ((job (printer-job printer)))
    (case (and job (print-job-state job))
      ((:queued :running)
       (if-let (remaining (print-job-remaining-seconds job))
         (format nil "Der laufende Auftrag ist in ca. ~A fertig."
                 (print-duration-text remaining))
         "Wie lange der laufende Auftrag noch braucht, wird gerade gemessen."))
      (:draining "Der laufende Auftrag ist gleich fertig.")
      (t "Der Drucker meldet sich gleich wieder."))))

;;; The form
;;;
;;; What the screen has to say about the printer sits in a dynamic area, so that
;;; a visitor kept waiting watches the wait shorten instead of a frozen page.
;;; Everything the form is made of — the labels, the fields, the Enter key — is
;;; there only while there is something to print on, and the update cycle asks
;;; for the screen afresh when that changes.

(defparameter *info-color* "^t"
  "The dynamic info area is turquoise, as the fields it replaced were.")

(defun fotodruck-info-lines ()
  "The three lines that say what can be printed just now, or why nothing can."
  (mapcar (lambda (line) (if (plusp (length line))
                             (format nil "~A   ~A" *info-color* line)
                             ""))
          (let ((busy (find-if #'printer-busy-p *printers*)))
            (cond
              ((not (fotodruck-configured-p))
               (list "Der Fotodruck ist auf diesem System nicht konfiguriert." "" ""))
              ((ready-printers)
               (list "Foto-ID vom Beleg des Fotoautomaten eingeben und Auflösung wählen."
                     "" ""))
              (busy
               (list "Der Drucker ist zurzeit belegt."
                     (busy-printer-note busy)
                     "Diese Anzeige aktualisiert sich selbst."))
              (t
               (list "Zurzeit ist kein Drucker verbunden."
                     "Bitte später noch einmal versuchen." ""))))))

(lispf:define-dynamic-area-updater fotodruck info ()
  (fotodruck-info-lines))

(lispf:define-screen-update fotodruck (lbl-printer printer-sel pr1 pr2 pr3
                                       lbl-id photo-id)
  (setf lbl-printer "" pr1 "" pr2 "" pr3 ""
        lbl-id "")
  (let ((ready (ready-printers)))
    ;; What the update cycle compares against to notice the wait is over.
    (setf (lispf:session-property lispf:*session* :fotodruck-waiting) (null ready))
    (if ready
        (lispf:show-key :enter "Weiter")
        ;; Nothing to fill in when there is nothing to print on, so the cursor
        ;; waits on the line that says why rather than under the key labels.
        (progn
          (setf photo-id "" printer-sel "")
          (dolist (field '("photo-id" "printer-sel"))
            (lispf:set-field-attribute field :write nil))
          (lispf:set-cursor 4 3)))
    ;; One printer is not a choice: the field that would take the number says
    ;; which printer it is, in the column the id is typed in.
    (unless (rest ready)
      (lispf:set-field-attribute "printer-sel" :write nil))
    (when ready
      (setf lbl-id "Foto-ID"
            lbl-printer "Drucker")
      (if (rest ready)
          (setf pr1 (printer-choice-line ready 1)
                pr2 (printer-choice-line ready 2)
                pr3 (printer-choice-line ready 3))
          (setf printer-sel (printer-name (first ready)))))))

(defun preview-error (reason)
  "What to say when the preview cannot be fetched. Every reason is about the
id, which is all the form has asked for by then."
  (case reason
    (:unknown "Diese Foto-ID ist unbekannt")
    (:deleted "Dieses Foto wurde gelöscht")
    (:converting "Das Foto wird noch aufbereitet - bitte in einigen Minuten erneut versuchen")
    (:missing "Für dieses Foto gibt es keine Vorschau")
    (t "Die Fotoseite ist nicht erreichbar")))

(defun print-run-error (reason)
  "What to say when a run cannot be fetched, and the field to answer it in.
A photo the website will not give up is answered in the id; a photo it has but
not at this density is answered in the density."
  (case reason
    (:unknown (values "Diese Foto-ID ist unbekannt" "photo-id"))
    (:deleted (values "Dieses Foto wurde gelöscht" "photo-id"))
    (:converting (values "Das Foto wird noch aufbereitet - bitte in einigen Minuten erneut versuchen"
                         "photo-id"))
    (:missing (values "Für dieses Foto gibt es keinen Ausdruck in dieser Auflösung"
                      "res-sel"))
    (t (values "Die Fotoseite ist nicht erreichbar" "photo-id"))))

;;; Every complaint the form makes names the field it is about and leaves the
;;; cursor there, so a correction is typed where the eye already is. The field
;;; order must not be what decides this.

(lispf:define-key-handler fotodruck :enter (printer-sel photo-id)
  (let* ((ready (ready-printers))
         (printer (selected-item ready printer-sel)))
    (unless printer
      (when ready
        (lispf:set-cursor-to-field "printer-sel"))
      (lispf:application-error
       (if ready "Bitte Drucker wählen" "Zurzeit ist kein Drucker verfügbar")))
    ;; The id is written back in the shape it is read in, so a correction is
    ;; made to what the website will be asked for.
    (setf photo-id (string-upcase (string-trim " " photo-id)))
    (unless (valid-photo-id-p photo-id)
      (lispf:set-cursor-to-field "photo-id")
      (lispf:application-error "Bitte die sechsstellige Foto-ID vom Beleg eingeben"))
    (multiple-value-bind (lines reason) (fetch-preview photo-id)
      (unless lines
        (lispf:set-cursor-to-field "photo-id")
        (lispf:application-error (preview-error reason)))
      (setf (lispf:session-property lispf:*session* :print-photo-id) photo-id
            (lispf:session-property lispf:*session* :print-printer) (printer-stem printer)
            (lispf:session-property lispf:*session* :print-preview) lines)
      'fotodruck-vorschau)))

;;; The preview
;;;
;;; The photograph as the printer will lay it down, over the one line that asks
;;; how finely to print it: the visitor sees what they are paying a sheet for
;;; before they choose how long it takes.

(defun preview-display-lines (lines)
  "LINES set in from the left, so the picture stands in the middle of the screen."
  (let ((margin (make-string (floor (- 80 *preview-columns*) 2)
                             :initial-element #\Space)))
    (mapcar (lambda (line) (concatenate 'string margin line)) lines)))

(defun session-printer ()
  "The printer the form was answered for, while it is still there to print on."
  (when-let (stem (lispf:session-property lispf:*session* :print-printer))
    (find-printer stem)))

(lispf:define-dynamic-area-updater fotodruck-vorschau preview ()
  (preview-display-lines
   (lispf:session-property lispf:*session* :print-preview)))

(lispf:define-screen-update fotodruck-vorschau (lbl-res choices)
  (setf lbl-res "Auflösung"
        choices (if-let (printer (session-printer))
                  (resolution-choice-text (printer-resolutions printer))
                  ""))
  ;; The cursor waits under the density. The update answers with nothing: a
  ;; symbol here is read as a screen to go to instead of showing this one.
  (lispf:set-cursor-to-field "res-sel")
  nil)

(lispf:define-key-handler fotodruck-vorschau :enter (res-sel)
  (let ((printer (session-printer))
        (photo-id (lispf:session-property lispf:*session* :print-photo-id)))
    (unless printer
      (lispf:application-error "Zurzeit ist kein Drucker verfügbar"))
    (let ((resolution (selected-item (printer-resolutions printer) res-sel)))
      (unless resolution
        (lispf:set-cursor-to-field "res-sel")
        (lispf:application-error "Bitte Auflösung wählen"))
      (multiple-value-bind (data reason) (fetch-print-run printer resolution photo-id)
        (unless data
          (lispf:set-cursor-to-field "res-sel")
          (lispf:application-error (print-run-error reason)))
        (let ((job (start-print-job printer resolution
                                    (user-username (session-user lispf:*session*))
                                    data)))
          (unless job
            (lispf:application-error "Der Drucker ist inzwischen belegt"))
          (setf (lispf:session-property lispf:*session* :print-job) job)
          ;; The preview has done its job once the sheet is on its way, so the
          ;; status takes its place and the form it was reached from goes with
          ;; it: leaving the status lands where the form was called from.
          (pop (lispf:session-screen-stack lispf:*session*))
          (cons :replace 'fotodruck-status))))))

;;; The sheet coming out

(defun print-status-lines (job)
  (let ((printer (print-job-printer job))
        (resolution (print-job-resolution job)))
    (append
     (list ""
           (format nil "   Drucker     ~A" (printer-name printer))
           (format nil "   Auflösung   ~A (~D dpi)"
                   (print-resolution-label resolution)
                   (print-resolution-dpi resolution))
           "")
     (case (print-job-state job)
       ((:queued :running)
        (list (format nil "   Gedruckt    ~D%" (round (* 100 (print-job-fraction job))))
              (if-let (remaining (print-job-remaining-seconds job))
                (format nil "   Fertig in   ca. ~A" (print-duration-text remaining))
                "   Fertig in   wird gemessen")
              ""
              "   Der Ausdruck läuft auch weiter, wenn Du diese Anzeige verlässt."))
       (:draining
        (list "   Der Ausdruck ist gleich fertig."
              ""
              "   Die letzten Zeilen sind noch unterwegs zum Drucker."))
       (:done
        (list "   Der Ausdruck ist fertig."
              ""
              "   Das Blatt liegt im Drucker."))
       (t
        (list "   Der Ausdruck wurde abgebrochen."
              ""
              (format nil "   ~A" (or (print-job-message job) ""))))))))

(lispf:define-screen-update fotodruck-status ()
  ;; Nothing to type here, so the cursor waits at the head of the report rather
  ;; than under the key labels.
  (lispf:set-cursor 2 3))

(lispf:define-dynamic-area-updater fotodruck-status status ()
  (when-let (job (lispf:session-property lispf:*session* :print-job))
    (print-status-lines job)))

;;; Waiting for the printer
;;;
;;; The form is built from what the printer can do, so a wait that ends has to
;;; reach the screen as a fresh build rather than as new text in the info area.

(defun fotodruck-wait-is-over-p ()
  "Return T when the session sits on a Fotodruck form that has become usable."
  (and (eq (lispf:session-current-screen lispf:*session*) 'fotodruck)
       (lispf:session-property lispf:*session* :fotodruck-waiting)
       (ready-printers)
       t))
