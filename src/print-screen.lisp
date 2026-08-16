;;; -*- Mode: Lisp -*-

;;; Fotodruck screens: the form a visitor types their photo id into, and the
;;; live view of the sheet coming out.

(in-package #:veron)

(defun printer-choice-line (items index)
  "The INDEX-th (1-based) printer as a choice line, or an empty line."
  (if-let (printer (nth (1- index) items))
    (format nil "~D  ~A" index (printer-name printer))
    ""))

(defun resolution-choice-line (items index)
  "The INDEX-th (1-based) resolution as a choice line, or an empty line."
  (if-let (resolution (nth (1- index) items))
    (format nil "~D  ~8A (~3D dpi)  ca. ~D Minuten"
            index
            (print-resolution-label resolution)
            (print-resolution-dpi resolution)
            (print-resolution-minutes resolution))
    ""))

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

(defun busy-printer-note ()
  "What to say while a sheet is on its way to the only printer there is."
  (if-let (printer (find-if #'printer-busy-p (connected-printers)))
    (format nil "Der laufende Auftrag ist in ca. ~D Minuten fertig."
            (print-job-remaining-minutes (printer-job printer)))
    ""))

;;; The form

(lispf:define-screen-update fotodruck (info1 info2 info3
                                       lbl-printer printer-sel pr1 pr2 pr3
                                       lbl-id photo-id
                                       lbl-res res-sel rs1 rs2 rs3)
  (setf info1 "" info2 "" info3 ""
        lbl-printer "" pr1 "" pr2 "" pr3 ""
        lbl-id "" lbl-res "" rs1 "" rs2 "" rs3 "")
  (let ((ready (ready-printers)))
    (if ready
        (lispf:show-key :enter "Drucken")
        ;; Nothing to fill in when there is nothing to print on.
        (progn
          (setf photo-id "" res-sel "" printer-sel "")
          (dolist (field '("photo-id" "res-sel" "printer-sel"))
            (lispf:set-field-attribute field :write nil))))
    (unless (rest ready)
      (lispf:set-field-attribute "printer-sel" :write nil))
    (cond
      ((not (fotodruck-configured-p))
       (setf info1 "Der Fotodruck ist auf diesem System nicht konfiguriert."))
      ((null (connected-printers))
       (setf info1 "Zurzeit ist kein Drucker verbunden."
             info2 "Bitte spaeter noch einmal versuchen."))
      ((null ready)
       (setf info1 "Der Drucker ist zurzeit belegt."
             info2 (busy-printer-note)
             info3 "Bitte spaeter noch einmal versuchen."))
      (t
       (setf info1 "Foto-ID vom Beleg des Fotoautomaten eingeben und Aufloesung waehlen."
             lbl-id "Foto-ID"
             lbl-res "Aufloesung"
             lbl-printer "Drucker")
       (if (rest ready)
           (setf pr1 (printer-choice-line ready 1)
                 pr2 (printer-choice-line ready 2)
                 pr3 (printer-choice-line ready 3))
           (setf pr1 (printer-name (first ready))))
       (let ((resolutions (printer-resolutions
                           (or (selected-item ready printer-sel) (first ready)))))
         (setf rs1 (resolution-choice-line resolutions 1)
               rs2 (resolution-choice-line resolutions 2)
               rs3 (resolution-choice-line resolutions 3)))))))

(defun print-run-error-message (reason)
  (case reason
    (:unknown "Diese Foto-ID ist unbekannt")
    (:deleted "Dieses Foto wurde geloescht")
    (:converting "Das Foto wird noch aufbereitet - bitte in einigen Minuten erneut versuchen")
    (:missing "Fuer dieses Foto gibt es keinen Ausdruck in dieser Aufloesung")
    (t "Die Fotoseite ist nicht erreichbar")))

(lispf:define-key-handler fotodruck :enter (printer-sel photo-id res-sel)
  (let* ((ready (ready-printers))
         (printer (selected-item ready printer-sel)))
    (unless printer
      (lispf:application-error
       (if ready "Bitte Drucker waehlen" "Zurzeit ist kein Drucker verfuegbar")))
    (let ((id (string-upcase (string-trim " " photo-id)))
          (resolution (selected-item (printer-resolutions printer) res-sel)))
      (unless (valid-photo-id-p id)
        (lispf:application-error "Bitte die sechsstellige Foto-ID vom Beleg eingeben"))
      (unless resolution
        (lispf:application-error "Bitte Aufloesung waehlen"))
      (multiple-value-bind (data reason) (fetch-print-run printer resolution id)
        (unless data
          (lispf:application-error (print-run-error-message reason)))
        (let ((job (start-print-job printer resolution
                                    (user-username (session-user lispf:*session*))
                                    data)))
          (unless job
            (lispf:application-error "Der Drucker ist inzwischen belegt"))
          (setf (lispf:session-property lispf:*session* :print-job) job)
          'fotodruck-status)))))

;;; The sheet coming out

(defun print-status-lines (job)
  (let ((printer (print-job-printer job))
        (resolution (print-job-resolution job)))
    (append
     (list ""
           (format nil "   Drucker     ~A" (printer-name printer))
           (format nil "   Aufloesung  ~A (~D dpi)"
                   (print-resolution-label resolution)
                   (print-resolution-dpi resolution))
           "")
     (case (print-job-state job)
       (:running
        (list (format nil "   Gesendet    ~D%" (round (* 100 (print-job-fraction job))))
              (format nil "   Fertig in   ca. ~D Minuten" (print-job-remaining-minutes job))
              ""
              "   Der Ausdruck laeuft auch weiter, wenn Sie diese Anzeige verlassen."))
       (:done
        (list "   Der Ausdruck ist fertig."
              ""
              "   Das Blatt liegt im Drucker."))
       (t
        (list "   Der Ausdruck wurde abgebrochen."
              ""
              (format nil "   ~A" (or (print-job-message job) ""))))))))

(lispf:define-dynamic-area-updater fotodruck-status status ()
  (when-let (job (lispf:session-property lispf:*session* :print-job))
    (print-status-lines job)))
