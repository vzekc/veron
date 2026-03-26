;;; -*- Mode: Lisp -*-

(in-package #:veron)

;;; Server instance ID

(defun generate-uuid-v4 ()
  "Generate a random UUID v4 string."
  (let ((bytes (ironclad:random-data 16)))
    (setf (aref bytes 6) (logior (logand (aref bytes 6) #x0f) #x40)
          (aref bytes 8) (logior (logand (aref bytes 8) #x3f) #x80))
    (format nil "~(~{~2,'0X~}~)" (coerce bytes 'list))))

(defvar *server-instance-id* (generate-uuid-v4)
  "Unique ID for this server process, stored in login records.")

;;; Database connection

(defvar *db-params* nil
  "Postmodern connection parameters list.")

(defun db-params ()
  (or *db-params*
      (setf *db-params*
            (list (env "VERON_DB_NAME")
                  (env "VERON_DB_USER")
                  (env "VERON_DB_PASSWORD")
                  (env "VERON_DB_HOST")
                  :port (parse-integer (env "VERON_DB_PORT"))
                  :pooled-p t))))

(defmacro with-db (&body body)
  `(pomo:with-connection (db-params)
     ,@body))

;;; Migration runner

(defun migrations-directory ()
  (merge-pathnames #P"migrations/" (asdf:system-source-directory :veron)))

(defun ensure-migrations-table ()
  (unless (pomo:table-exists-p "schema_migrations")
    (pomo:execute
     "CREATE TABLE schema_migrations (
        version INTEGER PRIMARY KEY,
        applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )")))

(defun applied-versions ()
  (mapcar #'first
          (pomo:query "SELECT version FROM schema_migrations ORDER BY version")))

(defun migration-files ()
  (sort (append (directory (merge-pathnames "*.sql" (migrations-directory)))
                (directory (merge-pathnames "*.lisp" (migrations-directory))))
        #'string< :key #'namestring))

(defun migration-version (path)
  (parse-integer (subseq (pathname-name path) 0 (position #\- (pathname-name path)))))

(defun split-sql (sql)
  "Split SQL string on semicolons into individual non-empty statements."
  (loop for stmt in (uiop:split-string sql :separator ";")
        for trimmed = (string-trim '(#\Space #\Tab #\Newline #\Return) stmt)
        when (plusp (length trimmed))
          collect trimmed))

(defun run-migration-file (file)
  "Run a single migration file. SQL files execute statements, Lisp files are loaded."
  (if (string-equal (pathname-type file) "lisp")
      (load file)
      (pomo:with-transaction ()
        (dolist (stmt (split-sql (uiop:read-file-string file)))
          (pomo:execute stmt)))))

(defun run-migrations ()
  (ensure-migrations-table)
  (let ((applied (applied-versions)))
    (dolist (file (migration-files))
      (let ((version (migration-version file)))
        (unless (member version applied)
          (lispf:log-message :info "applying migration ~A" (pathname-name file))
          (run-migration-file file)
          (with-db
            (pomo:execute (format nil "INSERT INTO schema_migrations (version) VALUES (~D)"
                                  version))))))))

(defun initialize-db ()
  (with-db (run-migrations)))

;;; User persistence

(defun hash-password (password)
  "Return hex-encoded SHA-256 hash of PASSWORD."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256
                             (ironclad:ascii-string-to-byte-array password))))

(defun generate-password (&optional (length 12))
  "Generate a random alphanumeric password of LENGTH characters."
  (let ((chars "abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ23456789")
        (bytes (ironclad:random-data length)))
    (map 'string (lambda (b) (char chars (mod b (length chars)))) bytes)))

(defun next-local-user-id ()
  "Return the next available negative ID for local users."
  (with-db
    (let ((min-id (pomo:query "SELECT COALESCE(MIN(id), 0) FROM users WHERE id < 0" :single)))
      (1- min-id))))

(defun create-local-user (username email)
  "Create a local-only user with USERNAME and EMAIL. Generates a random password.
Returns (values user-object password)."
  (let* ((password (generate-password))
         (id (next-local-user-id))
         (hashed (hash-password password)))
    (with-db
      (pomo:execute
       "INSERT INTO users (id, name, email, local_password, last_login) VALUES ($1, $2, $3, $4, CURRENT_TIMESTAMP)"
       id username email hashed))
    (lispf:log-message :info "created local user ~A (ID ~D)" username id)
    (values (make-instance 'user
                           :id id
                           :username username
                           :email email
                           :groups nil
                           :login-time (get-universal-time))
            password)))

(defun authenticate-local (username password)
  "Try local password authentication. Returns a plist like woltlab-login on success, NIL otherwise."
  (with-db
    (let ((row (pomo:query
                "SELECT id, name, local_password, email FROM users WHERE name = $1 AND local_password IS NOT NULL"
                username :row)))
      (when (and row (string= (third row) (hash-password password)))
        (list :user-id (first row)
              :username (second row)
              :email (or (fourth row) "")
              :groups nil)))))

(defun lookup-local-user (username)
  "Look up a local user by name. Returns a plist like woltlab-login, or NIL."
  (with-db
    (let ((row (pomo:query "SELECT id, name, email FROM users WHERE name = $1"
                            username :row)))
      (when row
        (list :user-id (first row)
              :username (second row)
              :email (or (third row) "")
              :groups nil)))))

(defun has-local-password-p (username)
  "Return T if USERNAME has a local password set."
  (with-db
    (pomo:query "SELECT EXISTS(SELECT 1 FROM users WHERE name = $1 AND local_password IS NOT NULL)"
                username :single)))

(defun save-local-password (user-id password)
  "Set the local password for a user."
  (with-db
    (pomo:execute "UPDATE users SET local_password = $1 WHERE id = $2"
                  (hash-password password) user-id)))

(defun store-otp (username code seconds)
  "Store an OTP code for USERNAME, valid for SECONDS from now."
  (with-db
    (pomo:execute
     "UPDATE users SET otp_code = $1, otp_expires = CURRENT_TIMESTAMP + make_interval(secs => $2) WHERE name = $3"
     code (coerce seconds 'double-float) username)))

(defun get-active-otp (username)
  "Return (VALUES code minutes-ago) for USERNAME's active OTP, or NIL if expired/absent.
MINUTES-AGO is how many minutes since the OTP was sent (based on 5min expiry window)."
  (with-db
    (let ((row (pomo:query
                "SELECT otp_code, EXTRACT(EPOCH FROM (CURRENT_TIMESTAMP - (otp_expires - INTERVAL '5 minutes')))::integer
                 FROM users WHERE name = $1 AND otp_expires > CURRENT_TIMESTAMP"
                username :row)))
      (when (and row (not (db-null-p (first row))))
        (values (first row)
                (max 0 (floor (second row) 60)))))))

(defun clear-otp (username)
  "Remove the OTP code for USERNAME."
  (with-db
    (pomo:execute "UPDATE users SET otp_code = NULL, otp_expires = NULL WHERE name = $1"
                  username)))

(defun user-db-roles (user-id)
  "Return the list of roles for a user from the database."
  (with-db
    (let ((result (pomo:query "SELECT roles FROM users WHERE id = $1"
                              user-id :single)))
      (if (or (null result) (db-null-p result))
          nil
          (coerce result 'list)))))

(defun set-user-roles (user-id roles)
  "Set the roles array for a user."
  (with-db
    (pomo:execute "UPDATE users SET roles = $1 WHERE id = $2"
                  (coerce roles 'vector) user-id)))

(defun add-user-role (user-id role)
  "Add a role to a user if not already present. ROLE is a keyword."
  (let ((role-string (string-downcase (symbol-name role))))
    (with-db
      (pomo:execute
       "UPDATE users SET roles = array_append(roles, $1) WHERE id = $2 AND NOT ($1 = ANY(roles))"
       role-string user-id))))

(defun remove-user-role (user-id role)
  "Remove a role from a user. ROLE is a keyword."
  (let ((role-string (string-downcase (symbol-name role))))
    (with-db
      (pomo:execute
       "UPDATE users SET roles = array_remove(roles, $1) WHERE id = $2"
       role-string user-id))))

(defun ensure-db-user (user)
  (with-db
    (pomo:execute
     "INSERT INTO users (id, name, last_login) VALUES ($1, $2, CURRENT_TIMESTAMP)
      ON CONFLICT (id) DO UPDATE SET name = $2, last_login = CURRENT_TIMESTAMP"
     (user-id user) (user-username user))))

(defun record-login (user &key terminal-type tls)
  (with-db
    (pomo:query
     "INSERT INTO logins (user_id, terminal_type, tls, server_instance_id) VALUES ($1, $2, $3, $4) RETURNING id"
     (user-id user) terminal-type tls *server-instance-id* :single)))

(defun record-logout (login-id)
  (when login-id
    (with-db
      (pomo:execute
       "UPDATE logins SET logout_at = CURRENT_TIMESTAMP WHERE id = $1"
       login-id))))

(defun close-orphaned-sessions ()
  "Close open login sessions from previous server instances.
Returns list of (user-id username) pairs for closed sessions."
  (with-db
    (let ((users (pomo:query
                  "SELECT DISTINCT u.id, u.name FROM logins l
                   JOIN users u ON l.user_id = u.id
                   WHERE l.logout_at IS NULL
                     AND (l.server_instance_id IS NULL OR l.server_instance_id != $1)"
                  *server-instance-id*)))
      (pomo:execute
       "UPDATE logins SET logout_at = CURRENT_TIMESTAMP
        WHERE logout_at IS NULL
          AND (server_instance_id IS NULL OR server_instance_id != $1)"
       *server-instance-id*)
      users)))

(defun db-null-p (value)
  "Return T if VALUE is a Postmodern NULL marker."
  (eq value :null))

;;; Display timezone

(defvar *display-timezone* nil
  "Timezone offset for displaying timestamps.
NIL means use the system's local timezone (handles DST automatically).
Numeric values: -1 = CET (UTC+1), -2 = CEST (UTC+2), etc.")

(defun decode-display-time (universal-time)
  "Decode a universal time using the display timezone."
  (decode-universal-time universal-time *display-timezone*))

(defun format-date (universal-time)
  "Format a universal time as DD.MM.YYYY in the display timezone."
  (if (or (null universal-time) (db-null-p universal-time))
      ""
      (multiple-value-bind (sec min hour day month year)
          (decode-display-time universal-time)
        (declare (ignore sec min hour))
        (format nil "~2,'0D.~2,'0D.~4D" day month year))))

(defun format-datetime (universal-time)
  "Format a universal time as DD.MM.YYYY HH:MM in the display timezone."
  (if (or (null universal-time) (db-null-p universal-time))
      ""
      (multiple-value-bind (sec min hour day month year)
          (decode-display-time universal-time)
        (declare (ignore sec))
        (format nil "~2,'0D.~2,'0D.~4D ~2,'0D:~2,'0D" day month year hour min))))

;;; Guestbook persistence

(defun add-guestbook-entry (author message)
  (with-db
    (pomo:execute
     "INSERT INTO guestbook (author, message) VALUES ($1, $2)"
     author message)))

(defun guestbook-entries (start count)
  (with-db
    (pomo:query
     "SELECT id, author, message, created_at FROM guestbook
      ORDER BY created_at DESC LIMIT $1 OFFSET $2"
     count start :plists)))

(defun guestbook-entry (id)
  (with-db
    (first (pomo:query
            "SELECT id, author, message, created_at FROM guestbook WHERE id = $1"
            id :plists))))

(defun guestbook-count ()
  (with-db
    (pomo:query "SELECT COUNT(*) FROM guestbook" :single)))

(defun delete-guestbook-entry (id)
  (with-db
    (pomo:execute "DELETE FROM guestbook WHERE id = $1" id)))

;;; Login log

(defun login-log-entries (start count)
  (with-db
    (pomo:query
     "SELECT u.name, l.login_at, l.logout_at, l.terminal_type, l.tls
      FROM logins l JOIN users u ON l.user_id = u.id
      ORDER BY l.login_at DESC LIMIT $1 OFFSET $2"
     count start :plists)))

(defun login-log-count ()
  (with-db
    (pomo:query "SELECT COUNT(*) FROM logins" :single)))

;;; Changelog read tracking

(defun changelog-unread-p (user-id)
  "Return T if the changelog has been modified since the user last read it."
  (with-db
    (pomo:query
     "SELECT EXISTS(
        SELECT 1 FROM files f, users u
        WHERE f.name = 'Changelog' AND f.owner_id IS NULL
          AND u.id = $1
          AND (u.changelog_read_at IS NULL OR f.modified_at > u.changelog_read_at)
          AND f.content != ''::bytea)"
     user-id :single)))

(defun mark-changelog-read (user-id)
  "Mark the changelog as read by setting changelog_read_at to now."
  (with-db
    (pomo:execute "UPDATE users SET changelog_read_at = CURRENT_TIMESTAMP WHERE id = $1"
                  user-id)))

;;; Chat persistence

(defun chat-channel-ids ()
  (with-db
    (pomo:query "SELECT id FROM chat_channels" :column)))

(defun chat-channel-messages (channel-id)
  (with-db
    (pomo:query
     "SELECT id, username, message, created_at, message_type
      FROM chat_messages
      WHERE channel_id = $1 ORDER BY id ASC"
     channel-id :plists)))

(defun default-chat-channel-id ()
  (with-db
    (pomo:query "SELECT id FROM chat_channels WHERE name = 'allgemein'" :single)))

(defun insert-chat-message (channel-id user-id username message message-type)
  "Insert a chat message and return its ID."
  (with-db
    (pomo:query
     "INSERT INTO chat_messages (channel_id, user_id, username, message, message_type)
      VALUES ($1, $2, $3, $4, $5) RETURNING id"
     channel-id user-id username message message-type :single)))