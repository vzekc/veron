;;; -*- Mode: Lisp -*-

;;; VERON - V.z.E.k.C. Electronic Retro Online Network
;;; Main application using LISPF framework with WoltLab authentication.

(in-package #:veron)

;;; Session class

(defclass veron-session (lspf:session)
  ((user :initform nil :accessor session-user)))

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
    (setf (session-user lspf:*session*) (make-user result))
    'main))

(lspf:define-key-handler login :pf3 ()
  :logoff)

;;; Main screen

(lspf:define-screen-update main (welcome-message)
  (setf welcome-message
        (format nil "Welcome, ~A!" (user-username (session-user lspf:*session*)))))

(lspf:define-key-handler main :pf3 ()
  :logoff)

;;; Server entry point

(defun start (&key (port 3270) (host "127.0.0.1"))
  "Start the VERON application on PORT."
  (lspf:start-application *veron-app* :port port :host host))
