;;; -*- Mode: Lisp -*-

;;; Test harness for VERON E2E tests.
;;; Manages temporary databases and the VERON application lifecycle.
;;; Uses a template database with migrations pre-applied for fast test DB creation.

(in-package #:veron-tests)

;;; Database connection parameters

(defun admin-db-params ()
  "Connection parameters for the admin database (used to create/drop test DBs)."
  (list (or (uiop:getenv "VERON_DB_NAME") "veron")
        (or (uiop:getenv "VERON_DB_USER") "veron")
        (or (uiop:getenv "VERON_DB_PASSWORD") "")
        (or (uiop:getenv "VERON_DB_HOST") "localhost")
        :port (parse-integer (or (uiop:getenv "VERON_DB_PORT") "5432"))
        :pooled-p nil))

(defun test-db-params (db-name)
  "Connection parameters for a test database."
  (let ((admin (admin-db-params)))
    (list db-name
          (second admin)
          (third admin)
          (fourth admin)
          :port (getf (cddddr admin) :port)
          :pooled-p nil)))

;;; Template database management

(defvar *template-db-name* nil
  "Name of the current template database, or NIL if not yet created.")

(defun generate-db-name (prefix)
  (format nil "~A_~D_~D" prefix (sb-posix:getpid)
          (mod (get-internal-real-time) 100000)))

(defun ensure-template-db ()
  "Create the template database if it doesn't exist. Returns the template name.
The template has all migrations applied and is ready to be cloned."
  (or *template-db-name*
      (let ((name (generate-db-name "veron_tmpl")))
        (pomo:with-connection (admin-db-params)
          (pomo:execute (format nil "CREATE DATABASE ~A" name)))
        (let ((veron::*db-params* (test-db-params name)))
          (veron::with-db (veron::run-migrations)))
        (pomo:clear-connection-pool)
        (setf *template-db-name* name))))

(defun drop-template-db ()
  "Drop the template database if it exists."
  (when *template-db-name*
    (pomo:with-connection (admin-db-params)
      (ignore-errors
       (pomo:execute
        (format nil "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = '~A'"
                *template-db-name*)))
      (ignore-errors
       (pomo:execute (format nil "DROP DATABASE IF EXISTS ~A" *template-db-name*))))
    (setf *template-db-name* nil)))

;;; Test database lifecycle

(defun create-test-db (db-name)
  "Create a test database by cloning the template."
  (let ((template (ensure-template-db)))
    (pomo:with-connection (admin-db-params)
      (pomo:execute (format nil "CREATE DATABASE ~A TEMPLATE ~A" db-name template)))))

(defun drop-test-db (db-name)
  "Drop a test database, terminating any active connections first."
  (pomo:with-connection (admin-db-params)
    (ignore-errors
     (pomo:execute
      (format nil "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = '~A'" db-name)))
    (ignore-errors
     (pomo:execute (format nil "DROP DATABASE IF EXISTS ~A" db-name)))))

(defmacro with-test-db ((db-name-var) &body body)
  "Create a temporary database from the template, execute BODY, then drop it.
Sets veron::*db-params* globally (so app threads see it) and restores on exit."
  (let ((saved-params (gensym "SAVED-PARAMS")))
    `(let ((,db-name-var (generate-db-name "veron_test"))
           (,saved-params veron::*db-params*))
       (create-test-db ,db-name-var)
       (unwind-protect
            (progn
              (setf veron::*db-params* (test-db-params ,db-name-var))
              ,@body)
         (setf veron::*db-params* ,saved-params)
         (pomo:clear-connection-pool)
         (drop-test-db ,db-name-var)))))

;;; Test user management

(defun create-test-user (username password &key (id 99999) roles)
  "Create a local user in the test database. ROLES is a list of keyword roles."
  (let ((role-strings (when roles
                        (map 'vector (lambda (r) (string-downcase (symbol-name r))) roles))))
    (veron::with-db
      (pomo:execute
       "INSERT INTO users (id, name, local_password, roles, last_login) VALUES ($1, $2, $3, $4, CURRENT_TIMESTAMP)"
       id username (veron::hash-password password) (or role-strings #())))))

;;; Combined harness: test DB + VERON app + s3270

(defmacro with-veron-app ((session-var &key (username "testuser") (password "testpass") roles)
                           &body body)
  "Start a VERON instance with a fresh test database and s3270 session.
Creates a test user with USERNAME/PASSWORD and binds SESSION-VAR to the s3270 session.
ROLES is an optional list of keyword roles (e.g. (:veron-administrator))."
  (let ((db-name-var (gensym "DB-NAME")))
    `(with-test-db (,db-name-var)
       (create-test-user ,username ,password ,@(when roles `(:roles ,roles)))
       (veron::load-chat-from-db)
       (veron::ensure-delivery-thread)
       (with-test-app (,session-var veron::*veron-app*)
         ,@body))))

;;; Screen observation

(defun wait-for-screen-match (session predicate &key (timeout 5.0))
  "Poll the screen until PREDICATE returns true for the full screen text.
PREDICATE receives a single string (all rows joined with newlines).
Returns T if matched, NIL if timed out."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (let* ((rows (screen-text session))
             (full (format nil "~{~A~^~%~}" rows)))
        (when (funcall predicate full)
          (return t)))
      (when (> (get-internal-real-time) deadline)
        (return nil))
      (bt:thread-yield))))

(defun wait-for-screen-contains (session text &key (timeout 5.0))
  "Wait until the screen contains TEXT. Returns T or NIL on timeout."
  (wait-for-screen-match session (lambda (full) (search text full)) :timeout timeout))

;;; Helpers

(defun login (session username password)
  "Log into the VERON application.
Non-TLS connections go through the login-local screen.
If redirected to the changelog screen, press Enter to reach MAIN."
  (assert-on-screen session "LOGIN")
  (type-text session username)
  (press-enter session)
  (assert-on-screen session "LOGIN-LOCAL")
  (type-text session password)
  (press-enter session)
  (let ((title-row (string-trim '(#\Space) (screen-row session 0))))
    (when (and (>= (length title-row) 9)
               (string= "CHANGELOG" (subseq title-row 0 9)))
      (press-enter session)))
  (assert-on-screen session "MAIN"))

(defun select-menu-item (session label)
  "Select a menu item by its label text. Finds the item number from the screen
and types it in the command field."
  (let* ((rows (screen-text session))
         (number nil))
    (dolist (row rows)
      (let ((trimmed (string-trim '(#\Space) row)))
        (when (and (> (length trimmed) 0)
                   (digit-char-p (char trimmed 0)))
          (let* ((space-pos (position #\Space trimmed))
                 (key (subseq trimmed 0 space-pos))
                 (rest (string-trim '(#\Space) (subseq trimmed space-pos))))
            (when (and (>= (length rest) (length label))
                       (string-equal label (subseq rest 0 (length label))))
              (setf number key)
              (return))))))
    (assert number () "Menu item ~S not found on screen" label)
    (move-cursor session 21 14)
    (erase-eof session)
    (type-text session number)
    (press-enter session)))

(defun navigate-to (session screen-name)
  "Navigate to SCREEN-NAME via the command field.
Waits for the target screen to appear (handles background update races)."
  (move-cursor session 21 14)
  (erase-eof session)
  (type-text session (string-downcase screen-name))
  (press-enter session)
  (unless (wait-for-screen-contains session (string-upcase screen-name) :timeout 3.0)
    (assert-on-screen session screen-name)))

;;; Additional sessions

(defmacro with-secondary-session ((session-var &key (username "testuser2") (password "testpass2")
                                                 (id 99998))
                                &body body)
  "Create a second user and s3270 session against the running test app.
Must be used inside with-veron-app."
  (let ((s3270-var (gensym "S3270")))
    `(progn
       (create-test-user ,username ,password :id ,id)
       (let ((,s3270-var (launch-s3270)))
         (s3270-connect ,s3270-var "127.0.0.1" *test-app-port*)
         (unwind-protect
              (let ((,session-var ,s3270-var))
                ,@body)
           (ignore-errors (s3270-disconnect ,s3270-var))
           (ignore-errors (close-s3270 ,s3270-var)))))))

;;; LU config helpers

(defun enable-default-disconnect ()
  "Set the DEFAULT LU config to disconnect=true for tests that need the GOODBYE screen."
  (veron::with-db
    (pomo:execute "UPDATE lu_config SET disconnect = TRUE WHERE name = 'DEFAULT'")))

;;; Screen data

(defvar *login-screen* nil)
(defvar *login-local-screen* nil)
(defvar *guestbook-new-screen* nil)

(defun load-screen-data ()
  (let ((dir (merge-pathnames #P"screens/"
                              (asdf:system-source-directory :veron))))
    (setf *login-screen* (load-test-screen-data (merge-pathnames "login.screen" dir))
          *login-local-screen* (load-test-screen-data (merge-pathnames "login-local.screen" dir))
          *guestbook-new-screen* (load-test-screen-data
                                  (merge-pathnames "guestbook-new.screen" dir)))))

;;; Suite fixtures

(define-suite-fixtures
  (:name :template-db
   :scope :suite
   :setup (ensure-template-db)
   :teardown (drop-template-db))
  (:name :screen-data
   :scope :suite
   :setup (load-screen-data)))
