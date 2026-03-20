;;; -*- Mode: Lisp -*-

;;; VERON - V.z.E.k.C. Electronic Retro Online Network
;;; Main application using LISPF framework with WoltLab authentication.

(defpackage #:veron
  (:use #:cl)
  (:local-nicknames (#:lspf #:lispf)
                    (#:wl #:woltlab-login))
  (:export #:start))

(in-package #:veron)

(defun load-dotenv ()
  "Load variables from .env file in the system source directory, if it exists.
Does not override variables already set in the environment."
  (let ((path (merge-pathnames #P".env" (asdf:system-source-directory :veron))))
    (when (probe-file path)
      (with-open-file (in path)
        (loop for line = (read-line in nil)
              while line
              do (let ((trimmed (string-trim '(#\Space #\Tab) line)))
                   (when (and (plusp (length trimmed))
                              (char/= (char trimmed 0) #\#))
                     (let ((pos (position #\= trimmed)))
                       (when pos
                         (let ((key (subseq trimmed 0 pos))
                               (val (subseq trimmed (1+ pos))))
                           (unless (uiop:getenv key)
                             (setf (uiop:getenv key) val))))))))))))

(load-dotenv)

(defun env (name)
  "Get an environment variable, signaling an error if not set."
  (or (uiop:getenv name)
      (error "Environment variable ~A is not set" name)))

;;; Session class

(defclass veron-session (lspf:session)
  ((user-id :initform nil :accessor session-user-id)
   (username :initform nil :accessor session-username)
   (email :initform nil :accessor session-email)
   (groups :initform nil :accessor session-groups)
   (login-time :initform nil :accessor session-login-time)))

;;; Application definition

(lspf:define-application *veron-app*
  :entry-screen login
  :screen-directory (merge-pathnames
                     #P"screens/"
                     (asdf:system-source-directory :veron))
  :session-class 'veron-session)

;;; Login screen

(lspf:define-key-handler login :enter (username password)
  (when (or (string= username "") (string= password ""))
    (lspf:application-error "Please enter username and password"))
  (let ((result (wl:authenticate-user username password
                                      :host (env "VERON_AUTH_DB_HOST")
                                      :port (parse-integer (env "VERON_AUTH_DB_PORT"))
                                      :database (env "VERON_AUTH_DB_NAME")
                                      :user (env "VERON_AUTH_DB_USER")
                                      :db-password (env "VERON_AUTH_DB_PASSWORD"))))
    (unless result
      (lspf:application-error "Invalid username or password"))
    (setf (session-user-id lspf:*session*) (getf result :user-id)
          (session-username lspf:*session*) (getf result :username)
          (session-email lspf:*session*) (getf result :email)
          (session-groups lspf:*session*) (getf result :groups)
          (session-login-time lspf:*session*) (get-universal-time))
    'main))

;;; Main screen

(lspf:define-screen-update main (welcome-message)
  (setf welcome-message
        (format nil "Welcome, ~A!" (session-username lspf:*session*))))

(lspf:define-key-handler main :pf3 ()
  :logoff)

;;; Server entry point

(defun start (&key (port 3270) (host "127.0.0.1"))
  "Start the VERON application on PORT."
  (lspf:start-application *veron-app* :port port :host host))
