;;; -*- Mode: Lisp -*-

;;; LU configuration screen - restricted by roles in screen definition

(in-package #:veron)

;;; LU config list screen

(lispf:define-list-data-getter lu-config (start end)
  (let* ((total (lu-config-count))
         (entries (list-lu-configs start (- end start))))
    (values (loop for e in entries
                  collect (list :name (getf e :name)
                                :description (or (getf e :description) "")
                                :no-disconnect (if (getf e :no-disconnect) "Nein" "Ja")
                                :single-instance (if (getf e :single-instance) "Ja" "Nein")
                                :allowed-ips (let ((ips (getf e :allowed-ips)))
                                               (if (or (null ips) (string= ips ""))
                                                   "" ips))))
            total)))

(lispf:define-key-handler lu-config :enter ()
  (let ((index (lispf:selected-list-index)))
    (when index
      (let* ((entries (list-lu-configs index 1))
             (entry (first entries)))
        (when entry
          (setf (lispf:session-property lispf:*session* :edit-lu-name)
                (getf entry :name))
          'lu-config-edit)))))

(lispf:define-key-handler lu-config :pf5 ()
  (setf (lispf:session-property lispf:*session* :edit-lu-name) nil)
  'lu-config-edit)

;;; IP validation

(defun validate-allowed-ips (allowed-ips)
  "Validate comma/space separated CIDR entries. Returns NIL if valid,
or the first invalid entry string if validation fails."
  (let ((trimmed (string-trim '(#\Space #\Newline) (or allowed-ips ""))))
    (when (plusp (length trimmed))
      (dolist (entry (split-sequence:split-sequence-if
                      (lambda (c) (or (char= c #\,) (char= c #\Space)
                                      (char= c #\Newline)))
                      trimmed :remove-empty-subseqs t))
        (let ((clean (string-trim '(#\Space) entry)))
          (unless (parse-cidr clean)
            (return clean)))))))

;;; LU config edit/new screen (unified)

(defun edit-lu-entry ()
  "Return the current edit LU config from the database."
  (find-lu-config-by-name (lispf:session-property lispf:*session* :edit-lu-name)))

(defun find-lu-config-by-name (name)
  "Look up exact LU configuration by name (no fallback to *)."
  (with-db
    (first (pomo:query
            "SELECT name, description, no_disconnect, single_instance, allowed_ips
               FROM lu_config WHERE name = $1" name :plists))))

(defun lu-config-new-p ()
  "Return T if the current LU config screen is in new mode."
  (not (lispf:session-property lispf:*session* :edit-lu-name)))

(lispf:define-screen-update lu-config-edit
    (screen-title lu-name description no-disconnect single-instance allowed-ips)
  (if (lu-config-new-p)
      (progn
        (setf screen-title "Neue LU Konfiguration"
              no-disconnect "J"
              single-instance "N")
        (lispf:set-field-attribute "lu-name" :write t)
        (lispf:show-key :pf5 "Anlegen"))
      (let ((entry (edit-lu-entry)))
        (setf screen-title "LU Konfiguration")
        (when entry
          (setf lu-name (getf entry :name)
                description (or (getf entry :description) "")
                no-disconnect (if (getf entry :no-disconnect) "J" "N")
                single-instance (if (getf entry :single-instance) "J" "N")
                allowed-ips (or (getf entry :allowed-ips) ""))
          (lispf:show-key :pf5 "Speichern")
          (unless (string= (getf entry :name) "*")
            (lispf:show-key :pf9 "Loeschen"))))))

(lispf:define-key-handler lu-config-edit :pf3 ()
  :back)

(lispf:define-key-handler lu-config-edit :pf5
    (lu-name description no-disconnect single-instance allowed-ips)
  (alexandria:when-let (bad-ip (validate-allowed-ips allowed-ips))
    (lispf:application-error (format nil "Ungueltige IP: ~A" bad-ip)))
  (if (lu-config-new-p)
      (let ((name (string-trim '(#\Space) (or lu-name ""))))
        (when (string= name "")
          (lispf:application-error "Bitte LU Name eingeben"))
        (when (find-lu-config-by-name name)
          (lispf:application-error "LU existiert bereits"))
        (add-lu-config name
                       (string-trim '(#\Space) (or description ""))
                       (field-enabled-p no-disconnect)
                       (field-enabled-p single-instance)
                       (string-trim '(#\Space) (or allowed-ips "")))
        (setf (lispf:list-offset lispf:*session* 'lu-config) 0
              (gethash "%errormsg" (lispf:session-context lispf:*session*))
              (format nil "LU ~A angelegt" name))
        :back)
      (let ((name (lispf:session-property lispf:*session* :edit-lu-name)))
        (unless (find-lu-config-by-name name)
          (lispf:application-error "LU Konfiguration nicht gefunden"))
        (update-lu-config name
                          (string-trim '(#\Space) (or description ""))
                          (field-enabled-p no-disconnect)
                          (field-enabled-p single-instance)
                          (string-trim '(#\Space) (or allowed-ips "")))
        (setf (gethash "%errormsg" (lispf:session-context lispf:*session*))
              "Gespeichert")
        :stay)))

(lispf:define-key-handler lu-config-edit :pf9 ()
  (when (lu-config-new-p)
    (lispf:application-error "Funktion nicht verfuegbar"))
  (let ((name (lispf:session-property lispf:*session* :edit-lu-name)))
    (when (string= name "*")
      (lispf:application-error "Standard LU kann nicht geloescht werden"))
    (lispf:request-confirmation
     (format nil "LU ~A loeschen?" name)
     (lambda ()
       (delete-lu-config name)
       (setf (lispf:list-offset lispf:*session* 'lu-config) 0
             (gethash "%errormsg" (lispf:session-context lispf:*session*))
             (format nil "LU ~A geloescht" name))
       :back))))
