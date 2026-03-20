;;; -*- Mode: Lisp -*-

;;; VERON - V.z.E.k.C. Electronic Retro Online Network
;;; Main application using LISPF framework with WoltLab authentication.

(in-package #:veron)

;;; Session class

(defclass veron-session (lspf:session)
  ((user :initform nil :accessor session-user)
   (term-type :initform "" :accessor session-term-type)
   (connect-time :initform (get-universal-time) :reader session-connect-time)))

;;; Application definition

(lspf:define-application *veron-app*
  :entry-screen login
  :screen-directory (merge-pathnames
                     #P"screens/"
                     (asdf:system-source-directory :veron))
  :session-class 'veron-session)

;;; Utility

(defun format-duration (seconds)
  "Format a duration in seconds as a human-readable string."
  (let ((hours (floor seconds 3600))
        (minutes (floor (mod seconds 3600) 60)))
    (cond
      ((>= hours 1) (format nil "~Dh ~Dm" hours minutes))
      ((>= minutes 1) (format nil "~Dm" minutes))
      (t "<1m"))))

;;; Login screen

(lspf:define-screen-update login ()
  (when (and lspf:*device-info* (string= "" (session-term-type lspf:*session*)))
    (setf (session-term-type lspf:*session*) (cl3270::term-type lspf:*device-info*))))

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

(lspf:define-key-handler main :pf6 ()
  'about)

;;; About screen

(lspf:define-key-handler about :pf3 ()
  :back)

;;; Who's online screen

(defun session-list ()
  "Return a list of plists describing all active sessions."
  (let ((now (get-universal-time))
        (sessions '()))
    (bt:with-lock-held ((lspf::application-sessions-lock lspf:*application*))
      (dolist (s (lspf::application-sessions lspf:*application*))
        (let ((user (session-user s)))
          (push (list :user (if user (user-username user) "(login)")
                      :screen (string-downcase
                               (string (lspf:session-current-screen s)))
                      :connected (format-duration
                                  (- now (session-connect-time s))))
                sessions))))
    (nreverse sessions)))

(lspf:define-list-data-getter who (start end)
  (let* ((all (session-list))
         (total (length all))
         (page (subseq all start (min end total))))
    (values (loop for rec in page
                  for i from start
                  collect (list* :num (format nil "~D." (1+ i)) rec))
            total)))

;;; Server entry point

(defun start (&key (port 3270) (host "127.0.0.1"))
  "Start the VERON application on PORT."
  (lspf:start-application *veron-app* :port port :host host))
