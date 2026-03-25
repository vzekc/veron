;;; -*- Mode: Lisp -*-

;;; VERON - V.z.E.k.C. Electronic Retro Online Network
;;; Main application using LISPF framework with WoltLab authentication.

(in-package #:veron)

;;; Integrate woltlab-login logging with lispf logger

(setf wl:*log-function* #'lspf:log-message)

;;; Admin group configuration

(let ((groups-env (uiop:getenv "VERON_ADMIN_GROUPS")))
  (when groups-env
    (setf *admin-groups*
          (mapcar (lambda (s) (string-trim '(#\Space) s))
                  (uiop:split-string groups-env :separator ",")))))

;;; Session class

(defclass veron-session (editor:editor-session)
  ((user :initform nil :accessor session-user)
   (term-type :initform "" :accessor session-term-type)
   (connect-time :initform (get-universal-time) :reader session-connect-time)
   (login-id :initform nil :accessor session-login-id)
   (otp-code :initform nil :accessor session-otp-code)
   (otp-expires :initform nil :accessor session-otp-expires)
   (otp-username :initform nil :accessor session-otp-username)
   (otp-email-masked :initform nil :accessor session-otp-email-masked)
   (otp-attempts :initform 0 :accessor session-otp-attempts)
   (force-set-password :initform nil :accessor session-force-set-password)))

;;; Application definition

(lspf:define-application *veron-app*
  :name "veron"
  :title "VERON"
  :entry-screen login
  :screen-directory (merge-pathnames
                     #P"screens/"
                     (asdf:system-source-directory :veron))
  :session-class 'veron-session)

;;; Session cleanup - called for all disconnection paths

(defmethod lspf:session-cleanup ((app (eql *veron-app*)) session)
  (let ((login-id (session-login-id session)))
    (when login-id
      (record-logout login-id))))

;;; Application customization

(defmethod lspf:unknown-key-message ((app (eql *veron-app*)) key-name)
  (format nil "~A: Unbekannte Taste" key-name))

(defmethod lspf:unknown-command-message ((app (eql *veron-app*)) command)
  (format nil "~A: Unbekannter Befehl" command))

(defmethod lspf:default-command-label ((app (eql *veron-app*)))
  "Befehl   ==>")

(defmethod lspf:menu-command-label ((app (eql *veron-app*)))
  "Auswahl ==>")

(defmethod lspf:paging-labels ((app (eql *veron-app*)))
  (values "Vor." "Naech."))

(defmethod lspf:menu-key-labels ((app (eql *veron-app*)))
  (values "Auswahl" "Abmelden"))

(defmethod lspf:session-authenticated-p ((app (eql *veron-app*)) session)
  (not (null (session-user session))))

(defmethod lspf:anonymous-access-denied-message ((app (eql *veron-app*)))
  "Anmeldung erforderlich")


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

(lspf:define-screen-update login (password password-label)
  (when (and lspf:*device-info* (string= "" (session-term-type lspf:*session*)))
    (setf (session-term-type lspf:*session*) (cl3270::term-type lspf:*device-info*)))
  (setf password "")
  (let ((tls-p (lspf:session-tls-p lspf:*session*)))
    (if tls-p
        (setf password-label "Password:")
        (progn
          (setf password-label "")
          (lspf:set-field-attribute "password" :write nil)
          (when (not (gethash "errormsg" (lspf:session-context lspf:*session*)))
            (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
                  "Warnung: Verbindung ist nicht verschluesselt!"))))))

(lspf:define-key-handler login :enter (username password)
  (when (string= username "")
    (lspf:application-error "Bitte Benutzername eingeben"))
  (let ((tls-p (lspf:session-tls-p lspf:*session*)))
    (cond
      ;; TLS: cursor on username row → move to password field
      ((and tls-p (<= (lspf:cursor-row) 19) (string= password ""))
       (lspf:set-cursor 20 13)
       :stay)
      ;; TLS: standard WoltLab login
      (tls-p
       (when (string= password "")
         (lspf:application-error "Bitte Passwort eingeben"))
       (let ((result (or (authenticate-local username password)
                         (when (env "VERON_AUTH_DB_HOST" nil)
                           (wl:authenticate-user username password
                                                 :host (env "VERON_AUTH_DB_HOST")
                                                 :port (parse-integer (env "VERON_AUTH_DB_PORT"))
                                                 :database (env "VERON_AUTH_DB_NAME")
                                                 :user (env "VERON_AUTH_DB_USER")
                                                 :db-password (env "VERON_AUTH_DB_PASSWORD"))))))
         (unless result
           (lspf:application-error "Ungueltiger Benutzername oder Passwort"))
         (complete-login result)
         'main))
      ;; Non-TLS: user has local password
      ((has-local-password-p username)
       (setf (session-otp-username lspf:*session*) username)
       'login-local)
      ;; Non-TLS: no local password, send OTP
      (t
       (prepare-otp-login username)
       'login-otp))))

(lspf:define-key-handler login :pf3 ()
  :logoff)

;;; Login with local password screen

(lspf:define-screen-update login-local (username password)
  (setf username (or (session-otp-username lspf:*session*) "")
        password ""))

(lspf:define-key-handler login-local :enter (password)
  (login-with-local-password (session-otp-username lspf:*session*) password))

(lspf:define-key-handler login-local :pf3 ()
  'login)

;;; Login with OTP screen

(lspf:define-screen-update login-otp (username email-display otp-code)
  (let ((session lspf:*session*))
    (setf username (or (session-otp-username session) "")
          email-display (or (session-otp-email-masked session) "")
          otp-code "")))

(lspf:define-key-handler login-otp :enter (otp-code)
  (verify-otp (session-otp-username lspf:*session*) otp-code))

(lspf:define-key-handler login-otp :pf3 ()
  'login)

;;; Set password screen

(lspf:define-screen-update set-password (new-password confirm-password)
  (setf new-password "" confirm-password "")
  (when (session-force-set-password lspf:*session*)
    (lspf:show-key :pf3 "Abmelden")
    (when (not (gethash "errormsg" (lspf:session-context lspf:*session*)))
      (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
            "Bitte lokales Passwort setzen"))))

(lspf:define-key-handler set-password :enter (new-password confirm-password)
  (when (string= new-password "")
    (lspf:application-error "Bitte Passwort eingeben"))
  (when (< (length new-password) 6)
    (lspf:application-error "Passwort muss mindestens 6 Zeichen lang sein"))
  (unless (string= new-password confirm-password)
    (lspf:application-error "Passwoerter stimmen nicht ueberein"))
  (let ((user (session-user lspf:*session*)))
    (save-local-password (user-id user) new-password)
    (setf (session-force-set-password lspf:*session*) nil)
    (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
          "Passwort gespeichert"))
  'main)

(lspf:define-key-handler set-password :pf3 ()
  (if (session-force-set-password lspf:*session*)
      'goodbye
      :back))

;;; Main screen

(lspf:define-key-handler main :pf3 ()
  'logout)

;;; Logout confirmation

(lspf:define-key-handler logout :pf5 ()
  (let ((user (session-user lspf:*session*)))
    (when user
      (notify :logout "Abmeldung"
              (format nil "~A hat sich abgemeldet" (user-username user)))))
  'goodbye)

;;; Goodbye screen - display for 5 seconds, then disconnect

(lspf:define-key-handler goodbye :pf3 ()
  :logoff)

(defmethod lspf:session-idle-timeout ((app (eql *veron-app*)) session)
  (case (lspf:session-current-screen session)
    (login 60)
    (login-local 60)
    (login-otp 300)
    (goodbye 5)
    (otherwise nil)))

;;; About screen

(lspf:define-key-handler about :pf3 ()
  :back)

;;; Notizen (editor demo)

(lspf:define-screen-update notes ()
  (let* ((user (session-user lspf:*session*))
         (file-id (ensure-notes-file user))
         (path (file-to-disk file-id)))
    (editor:edit-file path :display-name "Notizen")
    (disk-to-file file-id)
    (cleanup-tmp-file file-id)
    :back))

;;; Notification settings screen

(defun field-enabled-p (value)
  "Return T if a checkbox field value represents 'enabled' (any non-blank char)."
  (and value (plusp (length (string-trim '(#\Space) value)))))

(defun event-checked (events event)
  "Return \"x\" if EVENT keyword is in the EVENTS list, empty string otherwise."
  (if (member event events) "x" ""))

(lspf:define-screen-update notifications
    (topic evt-guestbook evt-login evt-logout)
  (let* ((user (session-user lspf:*session*))
         (subs (user-subscriptions (user-id user)))
         (sub (first subs)))
    (when sub
      (let ((events (getf sub :events)))
        (setf topic (getf sub :topic)
              evt-guestbook (event-checked events :guestbook)
              evt-login (event-checked events :login)
              evt-logout (event-checked events :logout))))))

(lspf:define-key-handler notifications :pf5
    (topic evt-guestbook evt-login evt-logout)
  (let* ((user (session-user lspf:*session*))
         (topic-name (string-trim '(#\Space) topic))
         (events '()))
    (when (field-enabled-p evt-guestbook)
      (push :guestbook events))
    (when (field-enabled-p evt-login)
      (push :login events))
    (when (field-enabled-p evt-logout)
      (push :logout events))
    ;; Delete existing subscriptions for this user
    (dolist (sub (user-subscriptions (user-id user)))
      (unsubscribe (getf sub :id) (user-id user)))
    ;; Create new subscription if topic and events are set
    (cond
      ((and (string= topic-name "") (null events))
       (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
             "Benachrichtigungen deaktiviert"))
      ((string= topic-name "")
       (lspf:application-error "Bitte ntfy-Topic eingeben"))
      ((null events)
       (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
             "Benachrichtigungen deaktiviert"))
      (t
       (subscribe (user-id user) topic-name (nreverse events))
       (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
             "Gespeichert")))
    :stay))

;;; Login log screen

(lspf:define-list-data-getter log (start end)
  (let* ((total (login-log-count))
         (entries (login-log-entries start (- end start))))
    (values (loop for e in entries
                  collect (list :name (getf e :name)
                                :login-at (let ((ts (getf e :login-at)))
                                            (if ts (format-datetime ts) ""))
                                :logout-at (let ((ts (getf e :logout-at)))
                                             (if ts (format-datetime ts) ""))
                                :terminal-type (or (getf e :terminal-type) "")))
            total)))

;;; Server entry point

(defun start (&key (port 3270) (host "127.0.0.1")
                    tls-port certificate-file key-file key-password (starttls t))
  "Start the VERON application on PORT.
When CERTIFICATE-FILE and KEY-FILE are provided, TLS is available.
TLS-PORT enables a dedicated TLS listener. STARTTLS (default T) offers
STARTTLS negotiation on the plain port."
  (initialize-db)
  (load-chat-from-db)
  (lspf:start-application *veron-app* :port port :host host
                           :tls-port tls-port
                           :certificate-file certificate-file
                           :key-file key-file
                           :key-password key-password
                           :starttls starttls))

(defun maybe-start-swank ()
  "Start a Swank server if SWANK_PORT is set in the environment.
Overrides quit-lisp so that ,q in SLIME closes the connection
instead of killing the server process."
  (alexandria:when-let (port-string (env "SWANK_PORT" nil))
    (let ((port (parse-integer port-string)))
      (handler-bind ((style-warning #'muffle-warning))
        (swank:create-server :port port :dont-close t))
      (setf (fdefinition (find-symbol "QUIT-LISP" :swank/backend))
            (lambda ()
              (let ((restart (find-restart (find-symbol "CLOSE-CONNECTION" :swank))))
                (when restart (invoke-restart restart)))))
      (format t "~&;;; Swank server started on port ~D~%" port))))

(defun start-from-env ()
  "Start the VERON application with parameters read from environment variables.
Recognized variables (all optional, defaults in parentheses):
  VERON_HOST (\"0.0.0.0\"), VERON_PORT (3270),
  VERON_TLS_PORT, VERON_TLS_CERT, VERON_TLS_KEY,
  VERON_TLS_KEY_PASSWORD, VERON_STARTTLS (\"true\"),
  SWANK_PORT (disabled) - start a Swank server on this port."
  (maybe-start-swank)
  (let ((host (env "VERON_HOST" "0.0.0.0"))
        (port (parse-integer (env "VERON_PORT" "3270")))
        (tls-port (let ((v (env "VERON_TLS_PORT" nil)))
                    (when v (parse-integer v))))
        (cert (env "VERON_TLS_CERT" nil))
        (key (env "VERON_TLS_KEY" nil))
        (key-pw (env "VERON_TLS_KEY_PASSWORD" nil))
        (starttls (string-equal (env "VERON_STARTTLS" "true") "true")))
    (start :host host :port port
           :tls-port tls-port
           :certificate-file cert
           :key-file key
           :key-password key-pw
           :starttls starttls)))


(defun reload ()
  "Hot-reload VERON code, run migrations, and refresh caches.
Called via Swank during deployment. Existing sessions continue running."
  (format t "~&;;; VERON: hot-reload starting~%")
  (dolist (sys '(:veron :lispf :lispf-edit :woltlab-login))
    (asdf:clear-system sys))
  (format t "~&;;; VERON: reloading code~%")
  (funcall (find-symbol "QUICKLOAD" :ql) :veron)
  ;; After quickload, reload-post-code is the new version
  (reload-post-code)
  (format t "~&;;; VERON: hot-reload complete~%")
  :ok)

(defun reload-post-code ()
  "Post-code-reload steps: migrations, chat, screen cache, menus."
  (format t "~&;;; VERON: running migrations~%")
  (initialize-db)
  (format t "~&;;; VERON: reloading chat from DB~%")
  (bt:with-lock-held (*chat-lock*)
    (load-chat-from-db))
  (format t "~&;;; VERON: refreshing screens and menus~%")
  (let ((lspf:*application* *veron-app*))
    (lspf:reload-all-screens)
    (lspf:load-application-menus *veron-app*)))
