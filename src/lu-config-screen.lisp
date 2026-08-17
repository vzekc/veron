;;; -*- Mode: Lisp -*-

;;; LU configuration screen - restricted by roles in screen definition

(in-package #:veron)

;;; LU config list screen

(defun format-lu-options (entry max-len)
  "Build a compact options string for an LU config list entry.
Flags are short keywords when set; IP filter shown as IP:<value>.
If the result exceeds MAX-LEN, truncate and append *."
  (let ((parts nil))
    (when (getf entry :disconnect)
      (push "DISC" parts))
    (when (getf entry :single-instance)
      (push "SINGLE" parts))
    (when (getf entry :secure)
      (push "SSL" parts))
    (when (getf entry :locked)
      (push "LOCK" parts))
    (when (getf entry :notify-all)
      (push "NTFY" parts))
    (let ((init (getf entry :init-screen)))
      (when (and init (not (string= init "")))
        (push (format nil "INIT:~A" init) parts)))
    (let ((ips (getf entry :allowed-ips)))
      (when (and ips (not (string= ips "")))
        (push (format nil "IP:~A" ips) parts)))
    (let ((result (format nil "~{~A~^, ~}" (nreverse parts))))
      (if (> (length result) max-len)
          (concatenate 'string (subseq result 0 (1- max-len)) "*")
          result))))

(lispf:define-list-data-getter lu-config (start end)
  (let* ((total (lu-config-count))
         (entries (list-lu-configs start (- end start))))
    (values (loop for e in entries
                  collect (list :name (getf e :name)
                                :description (or (getf e :description) "")
                                :options (format-lu-options e 26)))
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
  "Look up exact LU configuration by name, case-insensitive (no fallback)."
  (with-db
    (alexandria:when-let (row (first (pomo:query
                                      "SELECT name, description, disconnect, single_instance, allowed_ips, secure,
                                              init_screen, locked, notify_all
                                       FROM lu_config WHERE UPPER(name) = UPPER($1)" name :plists)))
      (sanitize-plist row))))

(defun lu-config-new-p ()
  "Return T if the current LU config screen is in new mode."
  (not (lispf:session-property lispf:*session* :edit-lu-name)))

(defparameter *lu-edit-field-names*
  '(description disconnect single-instance allowed-ips secure
    init-screen locked notify-all))

(lispf:define-screen-enter lu-config-edit
    (screen-title lu-name description disconnect single-instance allowed-ips secure
     init-screen locked notify-all)
  (if (lu-config-new-p)
      (setf screen-title "Neue LU Konfiguration"
            disconnect "N"
            single-instance "N"
            secure "N"
            locked "N"
            notify-all "N")
      (let ((entry (edit-lu-entry)))
        (setf screen-title "LU Konfiguration")
        (when entry
          (setf lu-name (getf entry :name)
                description (or (getf entry :description) "")
                disconnect (if (getf entry :disconnect) "J" "N")
                single-instance (if (getf entry :single-instance) "J" "N")
                allowed-ips (or (getf entry :allowed-ips) "")
                secure (if (getf entry :secure) "J" "N")
                init-screen (or (getf entry :init-screen) "")
                locked (if (getf entry :locked) "J" "N")
                notify-all (if (getf entry :notify-all) "J" "N")))))
  (apply #'snapshot-fields (if (lu-config-new-p)
                               (cons 'lu-name *lu-edit-field-names*)
                               *lu-edit-field-names*)))

(lispf:define-screen-update lu-config-edit ()
  (if (lu-config-new-p)
      (progn
        (lispf:set-field-attribute "lu-name" :write t)
        (lispf:show-key :pf5 "Anlegen"))
      (progn
        (lispf:show-key :pf5 "Speichern")
        (unless (string= (lispf:session-property lispf:*session* :edit-lu-name) "DEFAULT")
          (lispf:show-key :pf9 "Löschen")))))

(lispf:define-key-handler lu-config-edit :pf3 ()
  (confirm-if-dirty (if (lu-config-new-p)
                        (cons 'lu-name *lu-edit-field-names*)
                        *lu-edit-field-names*)
                    (lambda () :back)))

(defun validate-lu-name (name)
  "Validate that NAME is a valid LU name: uppercase letters and digits, starting with a letter.
Returns the upcased name, or signals an application error."
  (let ((upcased (string-upcase (string-trim '(#\Space) (or name "")))))
    (when (string= upcased "")
      (lispf:application-error "Bitte LU Name eingeben"))
    (unless (alpha-char-p (char upcased 0))
      (lispf:application-error "LU Name muss mit Buchstabe beginnen"))
    (unless (every (lambda (c) (or (alpha-char-p c) (digit-char-p c))) upcased)
      (lispf:application-error "LU Name: nur Buchstaben und Ziffern"))
    upcased))

(defun clean-init-screen (value)
  "Clean and validate init-screen field value. Returns NIL for empty, lowercase string otherwise."
  (let ((trimmed (string-trim '(#\Space) (or value ""))))
    (when (plusp (length trimmed))
      (string-downcase trimmed))))

(lispf:define-key-handler lu-config-edit :pf5
    (lu-name description disconnect single-instance allowed-ips secure
     init-screen locked notify-all)
  (alexandria:when-let (bad-ip (validate-allowed-ips allowed-ips))
    (lispf:application-error (format nil "Ungültige IP: ~A" bad-ip)))
  (let ((init (clean-init-screen init-screen)))
    (if (lu-config-new-p)
        (let ((name (validate-lu-name lu-name)))
          (when (find-lu-config-by-name name)
            (lispf:application-error "LU existiert bereits"))
          (add-lu-config name
                         (string-trim '(#\Space) (or description ""))
                         (field-enabled-p disconnect)
                         (field-enabled-p single-instance)
                         (string-trim '(#\Space) (or allowed-ips ""))
                         (field-enabled-p secure)
                         :init-screen init
                         :locked (field-enabled-p locked)
                         :notify-all (field-enabled-p notify-all))
          (setf (lispf:list-offset lispf:*session* 'lu-config) 0)
          (lispf:set-message :confirmation "LU ~A angelegt" name)
          :back)
        (let ((name (lispf:session-property lispf:*session* :edit-lu-name)))
          (unless (find-lu-config-by-name name)
            (lispf:application-error "LU Konfiguration nicht gefunden"))
          (update-lu-config name
                            (string-trim '(#\Space) (or description ""))
                            (field-enabled-p disconnect)
                            (field-enabled-p single-instance)
                            (string-trim '(#\Space) (or allowed-ips ""))
                            (field-enabled-p secure)
                            :init-screen init
                            :locked (field-enabled-p locked)
                            :notify-all (field-enabled-p notify-all))
          (apply #'snapshot-fields *lu-edit-field-names*)
          (lispf:set-message :confirmation "Gespeichert")
          :stay))))

(lispf:define-key-handler lu-config-edit :pf9 ()
  (when (lu-config-new-p)
    (lispf:application-error "Funktion nicht verfügbar"))
  (let ((name (lispf:session-property lispf:*session* :edit-lu-name)))
    (when (string= name "DEFAULT")
      (lispf:application-error "Standard LU kann nicht gelöscht werden"))
    (lispf:request-confirmation
     (format nil "LU ~A löschen?" name)
     (lambda ()
       (delete-lu-config name)
       (setf (lispf:list-offset lispf:*session* 'lu-config) 0)
       (lispf:set-message :confirmation "LU ~A gelöscht" name)
       :back))))
