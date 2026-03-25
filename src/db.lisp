;;; -*- Mode: Lisp -*-

(in-package #:veron)

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
  (sort (directory (merge-pathnames "*.sql" (migrations-directory)))
        #'string< :key #'namestring))

(defun migration-version (path)
  (parse-integer (subseq (pathname-name path) 0 (position #\- (pathname-name path)))))

(defun split-sql (sql)
  "Split SQL string on semicolons into individual non-empty statements."
  (loop for stmt in (uiop:split-string sql :separator ";")
        for trimmed = (string-trim '(#\Space #\Tab #\Newline #\Return) stmt)
        when (plusp (length trimmed))
          collect trimmed))

(defun run-migrations ()
  (ensure-migrations-table)
  (let ((applied (applied-versions)))
    (dolist (file (migration-files))
      (let ((version (migration-version file)))
        (unless (member version applied)
          (format t "Applying migration ~A~%" (pathname-name file))
          (pomo:with-transaction ()
            (dolist (stmt (split-sql (uiop:read-file-string file)))
              (pomo:execute stmt))
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

(defun authenticate-local (username password)
  "Try local password authentication. Returns a plist like woltlab-login on success, NIL otherwise."
  (with-db
    (let ((row (pomo:query
                "SELECT id, name, local_password FROM users WHERE name = $1 AND local_password IS NOT NULL"
                username :row)))
      (when (and row (string= (third row) (hash-password password)))
        (list :user-id (first row)
              :username (second row)
              :email ""
              :groups nil)))))

(defun ensure-db-user (user)
  (with-db
    (pomo:execute
     "INSERT INTO users (id, name, last_login) VALUES ($1, $2, CURRENT_TIMESTAMP)
      ON CONFLICT (id) DO UPDATE SET name = $2, last_login = CURRENT_TIMESTAMP"
     (user-id user) (user-username user))))

(defun record-login (user &key terminal-type)
  (with-db
    (pomo:query
     "INSERT INTO logins (user_id, terminal_type) VALUES ($1, $2) RETURNING id"
     (user-id user) terminal-type :single)))

(defun record-logout (login-id)
  (when login-id
    (with-db
      (pomo:execute
       "UPDATE logins SET logout_at = CURRENT_TIMESTAMP WHERE id = $1"
       login-id))))

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
     "SELECT u.name, l.login_at, l.logout_at, l.terminal_type
      FROM logins l JOIN users u ON l.user_id = u.id
      ORDER BY l.login_at DESC LIMIT $1 OFFSET $2"
     count start :plists)))

(defun login-log-count ()
  (with-db
    (pomo:query "SELECT COUNT(*) FROM logins" :single)))

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