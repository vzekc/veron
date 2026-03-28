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
  'lu-config-new)

;;; LU config edit screen

(defun edit-lu-entry ()
  "Return the current edit LU config from the database."
  (find-lu-config-by-name (lispf:session-property lispf:*session* :edit-lu-name)))

(defun find-lu-config-by-name (name)
  "Look up exact LU configuration by name (no fallback to *)."
  (with-db
    (first (pomo:query
            "SELECT name, description, no_disconnect, allowed_ips FROM lu_config
             WHERE name = $1" name :plists))))

(lispf:define-screen-update lu-config-edit (lu-name description no-disconnect single-instance allowed-ips)
  (let ((entry (edit-lu-entry)))
    (when entry
      (setf lu-name (getf entry :name)
            description (or (getf entry :description) "")
            no-disconnect (if (getf entry :no-disconnect) "J" "")
            single-instance (if (getf entry :single-instance) "J" "")
            allowed-ips (or (getf entry :allowed-ips) "")))
    (lispf:show-key :pf5 "Speichern")
    (unless (string= (getf entry :name) "*")
      (lispf:show-key :pf9 "Loeschen"))))

(lispf:define-key-handler lu-config-edit :pf3 ()
  :back)

(lispf:define-key-handler lu-config-edit :pf5 (description no-disconnect single-instance allowed-ips)
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
    :stay))

(lispf:define-key-handler lu-config-edit :pf9 ()
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

;;; New LU config screen

(lispf:define-screen-update lu-config-new (lu-name description no-disconnect single-instance allowed-ips)
  (declare (ignore lu-name description single-instance allowed-ips))
  (when (string= no-disconnect "")
    (setf no-disconnect "J")))

(lispf:define-key-handler lu-config-new :pf3 ()
  :back)

(lispf:define-key-handler lu-config-new :pf5 (lu-name description no-disconnect single-instance allowed-ips)
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
    :back))
