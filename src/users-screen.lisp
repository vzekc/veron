;;; -*- Mode: Lisp -*-

;;; User maintenance screen - restricted by roles in screen definition

(in-package #:veron)

;;; Database queries

(defun list-users (start count)
  "Return a list of user plists for display."
  (with-db
    (pomo:query
     "SELECT id, name, email, roles, last_login FROM users
      ORDER BY name ASC LIMIT $1 OFFSET $2"
     count start :plists)))

(defun user-count ()
  (with-db
    (pomo:query "SELECT COUNT(*) FROM users" :single)))

(defun find-user-by-name (name)
  "Look up a user by name. Returns a plist or NIL."
  (with-db
    (first (pomo:query
            "SELECT id, name, email, roles, last_login FROM users WHERE name = $1"
            name :plists))))

(defun find-user-by-id (id)
  "Look up a user by ID. Returns a plist or NIL."
  (with-db
    (first (pomo:query
            "SELECT id, name, email, roles, last_login FROM users WHERE id = $1"
            id :plists))))

(defun update-user-email (user-id email)
  (with-db
    (pomo:execute "UPDATE users SET email = $1 WHERE id = $2" email user-id)))

(defun delete-user (user-id)
  (with-db
    (pomo:execute "DELETE FROM users WHERE id = $1" user-id)))

(defun reset-user-password (user-id)
  "Reset a user's password to a new random one. Returns the new password."
  (let ((password (generate-password)))
    (with-db
      (pomo:execute "UPDATE users SET local_password = $1 WHERE id = $2"
                    (hash-password password) user-id))
    password))

(defun user-has-role-p (entry role)
  "Check if a user plist has a given role."
  (let ((roles (getf entry :roles)))
    (and roles (not (db-null-p roles))
         (member role (mapcar (lambda (r) (intern (string-upcase r) :keyword))
                              (coerce roles 'list))))))

(defun valid-email-p (text)
  "Basic email validation: contains @ with text on both sides."
  (let ((at-pos (position #\@ text)))
    (and at-pos (plusp at-pos) (< at-pos (1- (length text))))))

;;; User list screen

(lispf:define-list-data-getter users (start end)
  (let* ((total (user-count))
         (entries (list-users start (- end start))))
    (values (loop for e in entries
                  for email = (getf e :email)
                  for ts = (getf e :last-login)
                  collect (list :name (or (getf e :name) "")
                                :email (if (db-null-p email) "" (or email ""))
                                :admin (if (user-has-role-p e :veron-administrator) "Ja" "")
                                :last-login (if (or (null ts) (db-null-p ts))
                                                "" (format-datetime ts))))
            total)))

(lispf:define-key-handler users :enter ()
  (let ((index (lispf:selected-list-index)))
    (when index
      (let* ((entries (list-users index 1))
             (entry (first entries)))
        (when entry
          (setf (lispf:session-property lispf:*session* :edit-user-id)
                (getf entry :id))
          'user-edit)))))

(lispf:define-key-handler users :pf5 ()
  'user-new)

;;; User edit screen

(defun edit-user-entry ()
  "Return the current edit user entry from the database."
  (find-user-by-id (lispf:session-property lispf:*session* :edit-user-id)))

(lispf:define-screen-update user-edit (username email admin last-login user-id)
  (let ((entry (edit-user-entry)))
    (when entry
      (let ((e-email (getf entry :email))
            (ts (getf entry :last-login)))
        (setf username (or (getf entry :name) "")
              email (if (db-null-p e-email) "" (or e-email ""))
              admin (if (user-has-role-p entry :veron-administrator) "x" "")
              last-login (if (or (null ts) (db-null-p ts))
                             "" (format-datetime ts))
              user-id (format nil "~D" (getf entry :id)))))
    (lispf:show-key :pf5 "Speichern")
    (lispf:show-key :pf6 "Passwort")
    (lispf:show-key :pf9 "Loeschen")))

(lispf:define-key-handler user-edit :pf3 ()
  :back)

(lispf:define-key-handler user-edit :pf5 (email admin)
  (let* ((uid (lispf:session-property lispf:*session* :edit-user-id))
         (entry (edit-user-entry)))
    (unless entry
      (lispf:application-error "Benutzer nicht gefunden"))
    (let ((new-email (string-trim '(#\Space) email)))
      (when (and (plusp (length new-email)) (not (valid-email-p new-email)))
        (lispf:application-error "Ungueltige E-Mail-Adresse"))
      (update-user-email uid new-email)
      (if (field-enabled-p admin)
          (add-user-role uid :veron-administrator)
          (remove-user-role uid :veron-administrator))
      (setf (gethash "errormsg" (lispf:session-context lispf:*session*))
            "Gespeichert"))
    :stay))

(lispf:define-key-handler user-edit :pf6 ()
  (let* ((uid (lispf:session-property lispf:*session* :edit-user-id))
         (entry (edit-user-entry)))
    (unless entry
      (lispf:application-error "Benutzer nicht gefunden"))
    (lispf:request-confirmation
     (format nil "Kennwort fuer ~A zuruecksetzen?" (getf entry :name))
     (lambda ()
       (let ((password (reset-user-password uid)))
         (setf (gethash "errormsg" (lispf:session-context lispf:*session*))
               (format nil "Neues Passwort: ~A" password)))
       :stay))))

(lispf:define-key-handler user-edit :pf9 ()
  (let* ((uid (lispf:session-property lispf:*session* :edit-user-id))
         (entry (edit-user-entry))
         (current-user (session-user lispf:*session*)))
    (unless entry
      (lispf:application-error "Benutzer nicht gefunden"))
    (when (= uid (user-id current-user))
      (lispf:application-error "Eigenen Benutzer kann man nicht loeschen"))
    (lispf:request-confirmation
     (format nil "Benutzer ~A loeschen?" (getf entry :name))
     (lambda ()
       (delete-user uid)
       (setf (lispf:list-offset lispf:*session* 'users) 0
             (gethash "errormsg" (lispf:session-context lispf:*session*))
             (format nil "Benutzer ~A geloescht" (getf entry :name)))
       :back))))

;;; New user screen

(lispf:define-screen-update user-new (username email)
  (setf username ""
        email ""))

(lispf:define-key-handler user-new :pf3 ()
  :back)

(lispf:define-key-handler user-new :pf5 (username email)
  (let ((name (string-trim '(#\Space) username))
        (mail (string-trim '(#\Space) email)))
    (when (string= name "")
      (lispf:application-error "Bitte Benutzername eingeben"))
    (when (find-user-by-name name)
      (lispf:application-error "Benutzer existiert bereits"))
    (when (and (plusp (length mail)) (not (valid-email-p mail)))
      (lispf:application-error "Ungueltige E-Mail-Adresse"))
    (multiple-value-bind (user password) (create-local-user name mail)
      (declare (ignore user))
      (setf (lispf:list-offset lispf:*session* 'users) 0
            (gethash "errormsg" (lispf:session-context lispf:*session*))
            (format nil "Benutzer ~A angelegt - Passwort: ~A" name password))
      :back)))
