;;; -*- Mode: Lisp -*-

;;; User maintenance screen - admin only

(in-package #:veron)

;;; Database queries

(defun list-users (start count)
  "Return a list of user plists for display."
  (with-db
    (pomo:query
     "SELECT id, name, email, is_admin, last_login FROM users
      ORDER BY name ASC LIMIT $1 OFFSET $2"
     count start :plists)))

(defun user-count ()
  (with-db
    (pomo:query "SELECT COUNT(*) FROM users" :single)))

(defun find-user-by-name (name)
  "Look up a user by name. Returns a plist or NIL."
  (with-db
    (first (pomo:query
            "SELECT id, name, email, is_admin, last_login FROM users WHERE name = $1"
            name :plists))))

(defun find-user-by-id (id)
  "Look up a user by ID. Returns a plist or NIL."
  (with-db
    (first (pomo:query
            "SELECT id, name, email, is_admin, last_login FROM users WHERE id = $1"
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

;;; Admin guard

(defun require-admin ()
  "Check that the current user is an admin. Signals application-error if not."
  (unless (admin-p (session-user lspf:*session*))
    (lspf:application-error "Keine Berechtigung")))

;;; User list screen

(lspf:define-list-data-getter users (start end)
  (require-admin)
  (let* ((total (user-count))
         (entries (list-users start (- end start))))
    (values (loop for e in entries
                  for email = (getf e :email)
                  for ts = (getf e :last-login)
                  collect (list :name (or (getf e :name) "")
                                :email (if (db-null-p email) "" (or email ""))
                                :admin (if (getf e :is-admin) "Ja" "")
                                :last-login (if (or (null ts) (db-null-p ts))
                                                "" (format-datetime ts))))
            total)))

(lspf:define-key-handler users :enter ()
  (require-admin)
  (let ((index (lspf:selected-list-index)))
    (when index
      (let* ((entries (list-users index 1))
             (entry (first entries)))
        (when entry
          (setf (lspf:session-property lspf:*session* :edit-user-id)
                (getf entry :id))
          'user-edit)))))

(lspf:define-key-handler users :pf5 ()
  (require-admin)
  'user-new)

;;; User edit screen

(lspf:define-screen-update user-edit (username email admin last-login user-id)
  (require-admin)
  (let ((entry (find-user-by-id
                (lspf:session-property lspf:*session* :edit-user-id))))
    (when entry
      (let ((e-email (getf entry :email))
            (ts (getf entry :last-login)))
        (setf username (or (getf entry :name) "")
              email (if (db-null-p e-email) "" (or e-email ""))
              admin (if (getf entry :is-admin) "x" "")
              last-login (if (or (null ts) (db-null-p ts))
                             "" (format-datetime ts))
              user-id (format nil "~D" (getf entry :id)))))))

(lspf:define-key-handler user-edit :pf5 (email admin)
  (require-admin)
  (let* ((uid (lspf:session-property lspf:*session* :edit-user-id))
         (entry (find-user-by-id uid)))
    (unless entry
      (lspf:application-error "Benutzer nicht gefunden"))
    (let ((new-email (string-trim '(#\Space) email))
          (new-admin (field-enabled-p admin)))
      (update-user-email uid new-email)
      (set-user-admin (getf entry :name) new-admin)
      (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
            "Gespeichert"))
    :stay))

(lspf:define-key-handler user-edit :pf6 ()
  (require-admin)
  (let* ((uid (lspf:session-property lspf:*session* :edit-user-id))
         (entry (find-user-by-id uid)))
    (unless entry
      (lspf:application-error "Benutzer nicht gefunden"))
    (let ((password (reset-user-password uid)))
      (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
            (format nil "Neues Passwort: ~A" password)))
    :stay))

(lspf:define-key-handler user-edit :pf9 ()
  (require-admin)
  (let* ((uid (lspf:session-property lspf:*session* :edit-user-id))
         (entry (find-user-by-id uid))
         (current-user (session-user lspf:*session*)))
    (unless entry
      (lspf:application-error "Benutzer nicht gefunden"))
    (when (= uid (user-id current-user))
      (lspf:application-error "Eigenen Benutzer kann man nicht loeschen"))
    (delete-user uid)
    (setf (lspf:list-offset lspf:*session* 'users) 0)
    (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
          (format nil "Benutzer ~A geloescht" (getf entry :name)))
    :back))

;;; New user screen

(lspf:define-screen-update user-new (username email)
  (require-admin)
  (setf username ""
        email ""))

(lspf:define-key-handler user-new :pf5 (username email)
  (require-admin)
  (let ((name (string-trim '(#\Space) username))
        (mail (string-trim '(#\Space) email)))
    (when (string= name "")
      (lspf:application-error "Bitte Benutzername eingeben"))
    (when (find-user-by-name name)
      (lspf:application-error "Benutzer existiert bereits"))
    (let ((user (create-local-user name mail)))
      (setf (lspf:session-property lspf:*session* :edit-user-id)
            (user-id user))
      (setf (lspf:list-offset lspf:*session* 'users) 0)
      (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
            (format nil "Benutzer ~A angelegt - Passwort in Serverlog" name))
      'user-edit)))
