;;; -*- Mode: Lisp -*-

;;; VERON - V.z.E.k.C. Electronic Retro Online Network
;;; Main application using LISPF framework with WoltLab authentication.

(in-package #:veron)

;;; Integrate woltlab-login logging with lispf logger

(setf woltlab-login:*log-function* #'lispf:log-message)

;;; Admin group configuration

(let ((groups-env (uiop:getenv "VERON_ADMIN_GROUPS")))
  (when groups-env
    (setf *admin-groups*
          (mapcar (lambda (s) (string-trim '(#\Space) s))
                  (uiop:split-string groups-env :separator ",")))))

;;; Connection class

(defclass veron-connection (lispf:connection)
  ((term-type :initform "" :accessor connection-term-type)
   (connect-time :initform (get-universal-time) :reader connection-connect-time)
   (lu-name :initform nil :accessor connection-lu-name)))

;;; Session classes

(defclass veron-session (lispf-editor:editor-session)
  ())

(defclass anonymous-session (veron-session)
  ((otp-username :initform nil :accessor session-otp-username)
   (otp-attempts :initform 0 :accessor session-otp-attempts)
   (password-reset-sent :initform nil :accessor session-password-reset-sent)))

(defclass authenticated-session (veron-session)
  ((user :initarg :user :reader session-user)
   (login-id :initform nil :accessor session-login-id)))

(defmethod session-user ((session veron-session))
  "Return NIL for non-authenticated sessions."
  nil)

;;; Application definition

(lispf:define-application *veron-app*
  :name "veron"
  :title "VERON"
  :entry-screen login
  :screen-directory (merge-pathnames
                     #P"screens/"
                     (asdf:system-source-directory :veron))
  :session-class 'anonymous-session
  :connection-class 'veron-connection)

(lispf:register-global-hotkey *veron-app* :pf12 'noti)

;;; Session factory

(defmethod lispf:make-session ((app (eql *veron-app*)) connection data)
  (if data
      (let* ((user (make-user data))
             (session (make-instance 'authenticated-session
                                     :connection connection
                                     :user user)))
        (ensure-db-user user)
        (setf (session-login-id session)
              (record-login user
                            :terminal-type (connection-term-type connection)
                            :tls (lispf:connection-tls-p connection)))
        (lispf:log-message :info "login user=~A tls=~A"
                          (user-username user)
                          (if (lispf:connection-tls-p connection) "yes" "no"))
        (when (getf data :otp-login)
          (setf (lispf:session-property session :otp-login) t))
        session)
      (make-instance 'anonymous-session :connection connection)))

;;; Session cleanup - called for all disconnection paths

(defmethod lispf:session-cleanup ((app (eql *veron-app*)) (session authenticated-session))
  (let ((login-id (session-login-id session))
        (user (session-user session)))
    (when login-id
      (record-logout login-id))
    (when (and user (lispf:session-property session :chat-entered)
               (not (lispf:session-property session :chat-leaving)))
      (let ((channel-id (default-channel-id)))
        (when channel-id
          (alexandria:when-let (channel (find-channel channel-id))
            (leave-channel channel session))
          (add-chat-notification channel-id user
                                 "--- ~A hat den Chat verlassen"
                                 (user-username user)))))
    (when user
      (lispf:log-message :info "logout user=~A" (user-username user)))))

;;; LU connection validation

(defmethod lispf:validate-connection ((app (eql *veron-app*)) devinfo client-ip)
  (let* ((lu-name (cl3270:device-name devinfo))
         (config (when lu-name (find-lu-config lu-name))))
    (when (and lu-name (not config))
      (return-from lispf:validate-connection
        (format nil "unknown LU ~A from ~A" lu-name client-ip)))
    (when (and lu-name config (not (string= lu-name (getf config :name))))
      ;; find-lu-config fell back to DEFAULT — the requested LU doesn't exist
      (return-from lispf:validate-connection
        (format nil "unknown LU ~A from ~A" lu-name client-ip)))
    ;; No LU specified: look up DEFAULT config if it exists
    (unless config
      (setf config (find-lu-config "DEFAULT"))
      ;; If no DEFAULT config exists either, accept unconditionally
      (unless config
        (return-from lispf:validate-connection t)))
    (unless (ip-matches-allowed-p client-ip (getf config :allowed-ips))
      (return-from lispf:validate-connection
        (format nil "LU ~A not allowed from ~A"
                (or lu-name "DEFAULT") client-ip)))
    (when (and (getf config :secure) (not (cl3270:tls-p devinfo)))
      (return-from lispf:validate-connection
        (format nil "LU ~A requires TLS from ~A"
                (or lu-name "DEFAULT") client-ip)))
    (when (and lu-name (getf config :single-instance))
      (bt:with-lock-held ((lispf:application-connections-lock app))
        (dolist (conn (lispf:application-connections app))
          (when (equal lu-name (connection-lu-name conn))
            (return-from lispf:validate-connection
              (format nil "LU ~A already connected" lu-name))))))
    t))

(defun session-lu-config ()
  "Return the LU configuration for the current session.
Falls back to DEFAULT if no LU is specified."
  (let ((lu-name (cl3270:device-name lispf:*device-info*)))
    (find-lu-config (or lu-name "DEFAULT"))))

(defun session-disconnect-p ()
  "Return T if the current session's LU has disconnect enabled.
When true, the tn3270 connection ends after logout instead of resetting to login."
  (let ((config (session-lu-config)))
    (if config
        (getf config :disconnect)
        t)))

;;; Application customization

(defmethod lispf:unknown-key-message ((app (eql *veron-app*)) key-name)
  (format nil "~A: Unbekannte Taste" key-name))

(defmethod lispf:unknown-command-message ((app (eql *veron-app*)) command)
  (format nil "~A: Unbekannter Befehl" (string-upcase command)))

(defmethod lispf:invalid-menu-selection-message ((app (eql *veron-app*)) selection)
  (format nil "~A: Ungueltige Auswahl" selection))

(defmethod lispf:default-command-label ((app (eql *veron-app*)))
  "Befehl   ==>")

(defmethod lispf:menu-command-label ((app (eql *veron-app*)))
  "Auswahl ==>")

(defmethod lispf:paging-labels ((app (eql *veron-app*)))
  (values "Vor." "Naech."))

(defmethod lispf:menu-key-labels ((app (eql *veron-app*)) &optional menu-name)
  (values "Auswahl" (if (equal menu-name "main") "Abmelden" "Zurueck")))

(defmethod lispf:session-authenticated-p ((app (eql *veron-app*)) session)
  (typep session 'authenticated-session))

(defmethod lispf:anonymous-access-denied-message ((app (eql *veron-app*)))
  "Anmeldung erforderlich")

(defmethod lispf:session-user-roles ((app (eql *veron-app*)) (session authenticated-session))
  (effective-roles (session-user session)))

(defmethod lispf:session-user-roles ((app (eql *veron-app*)) session)
  nil)

(defmethod lispf:role-access-denied-message ((app (eql *veron-app*)))
  "Keine Berechtigung")


;;; Orphaned session cleanup

(defun close-orphaned-chat-sessions ()
  "Close open login sessions from previous server instances and add chat disconnect notifications."
  (let ((users (close-orphaned-sessions)))
    (when users
      (let ((channel-id (default-channel-id)))
        (when channel-id
          (let ((channel (find-or-create-channel channel-id)))
            (dolist (user-pair users)
              (destructuring-bind (user-id username) user-pair
                (let* ((msg-text (format nil "--- ~A wurde getrennt (Neustart)" username))
                       (db-id (insert-chat-message channel-id user-id username
                                                   msg-text "notification"))
                       (msg (list :id db-id
                                  :type :notification
                                  :message msg-text
                                  :created-at (get-universal-time))))
                  (bt:with-lock-held ((channel-lock channel))
                    (vector-push-extend msg (channel-messages-buffer channel))
                    (setf (channel-id-counter channel)
                          (max (channel-id-counter channel) db-id)))))))))
      (lispf:log-message :info "closed ~D orphaned session~:P" (length users)))))

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

(lispf:define-screen-update login (password password-label)
  (let ((conn (lispf:session-connection lispf:*session*)))
    ;; Initialize connection-level state from device info on first display
    (when (and lispf:*device-info* (string= "" (connection-term-type conn)))
      (setf (connection-term-type conn) (cl3270::term-type lispf:*device-info*)
            (connection-lu-name conn) (cl3270:device-name lispf:*device-info*)))
    ;; If this is an authenticated session (created after login via session-reset),
    ;; skip the login screen and go directly to post-login screen
    (when (typep lispf:*session* 'authenticated-session)
      (notify :login "Anmeldung"
              (format nil "~A hat sich angemeldet"
                      (user-username (session-user lispf:*session*)))
              :originator-user-id (user-id (session-user lispf:*session*)))
      (update-my-chat-indicator)
      (let ((otp-login-p (lispf:session-property lispf:*session* :otp-login)))
        (return-from lispf::prepare-screen
          (if otp-login-p 'set-password-otp (post-login-screen)))))
    (unless (session-disconnect-p)
      (lispf:hide-key :pf3))
    (setf password "")
    (cond
      ((lispf:connection-tls-p conn)
       (setf password-label "Passwort:")
       (lispf:show-key :pf2 "Passwort vergessen"))
      (t
       (setf password-label "")
       (lispf:set-field-attribute "password" :write nil)
       (unless (lispf:session-property lispf:*session* :message-line)
         (lispf:set-message :error "Warnung: Verbindung ist nicht verschluesselt!"))))))

(lispf:define-key-handler login :enter (username password)
  (when (string= username "")
    (lispf:application-error "Bitte Benutzername eingeben"))
  (let ((conn (lispf:session-connection lispf:*session*)))
    (cond
      ;; TLS: cursor on username row → move to password field
      ((and (lispf:connection-tls-p conn) (<= (lispf:cursor-row) 19) (string= password ""))
       (lispf:next-input-field))
      ;; TLS: standard WoltLab login, also accepts valid OTP as password
      ((lispf:connection-tls-p conn)
       (when (string= password "")
         (lispf:application-error "Bitte Passwort eingeben"))
       (let ((result (or (authenticate-local username password)
                         (when (env "VERON_AUTH_DB_HOST" nil)
                           (woltlab-login:authenticate-user username password
                                                 :host (env "VERON_AUTH_DB_HOST")
                                                 :port (parse-integer (env "VERON_AUTH_DB_PORT"))
                                                 :database (env "VERON_AUTH_DB_NAME")
                                                 :user (env "VERON_AUTH_DB_USER")
                                                 :db-password (env "VERON_AUTH_DB_PASSWORD")))
                         (authenticate-with-otp username password))))
         (unless result
           (lispf:application-error "Ungueltiger Benutzername oder Passwort"))
         (signal 'lispf:session-reset :data result)))
      ;; Non-TLS: user has local password
      ((has-local-password-p username)
       (setf (session-otp-username lispf:*session*) username)
       'login-local)
      ;; Non-TLS: WoltLab user without local password, send OTP
      ((lookup-woltlab-email username)
       (prepare-otp-login username)
       'login-otp)
      ;; Unknown user
      (t
       (lispf:application-error "Benutzer nicht gefunden")))))

(defun initiate-password-reset (username)
  "Initiate a password reset for USERNAME. Enforces one reset per session.
Sends an OTP email if the account exists, but always shows the same
confirmation message to avoid revealing whether the account exists."
  (let ((session lispf:*session*))
    (when (session-password-reset-sent session)
      (lispf:application-error "Passwort-Reset bereits angefordert"))
    (setf (session-password-reset-sent session) t
          (session-otp-username session) username)
    (let ((email (lookup-woltlab-email username)))
      (when email
        (ignore-errors (prepare-otp-login username))))
    'login-otp))

(lispf:define-key-handler login :pf2 (username)
  (when (string= username "")
    (lispf:application-error "Bitte Benutzername eingeben"))
  (initiate-password-reset username))

(lispf:define-key-handler login :pf3 ()
  :logoff)

;;; Login with local password screen

(lispf:define-screen-update login-local (username password)
  (setf username (or (session-otp-username lispf:*session*) "")
        password ""))

(lispf:define-key-handler login-local :enter (password)
  (login-with-local-password (session-otp-username lispf:*session*) password))

(lispf:define-key-handler login-local :pf3 ()
  'login)

(lispf:define-key-handler login-local :pf2 ()
  (initiate-password-reset (session-otp-username lispf:*session*)))

;;; Login with OTP screen

(lispf:define-screen-update login-otp (username otp-code)
  (let ((session lispf:*session*))
    (setf username (or (session-otp-username session) "")
          otp-code "")
    (lispf:set-message :confirmation "Falls ein Konto existiert, wurde eine E-Mail gesendet")))

(lispf:define-key-handler login-otp :enter (otp-code)
  (verify-otp (session-otp-username lispf:*session*) otp-code))

(lispf:define-key-handler login-otp :pf3 ()
  'login)

;;; Set password after OTP login (overlay)

(lispf:define-screen-update set-password-otp (new-password confirm-password)
  (let ((saved (lispf:session-property lispf:*session* :saved-password)))
    (setf new-password (or saved "")
          confirm-password ""))
  (lispf:set-message :confirmation "Bitte lokales Passwort setzen"))

(lispf:define-key-handler set-password-otp :enter (new-password confirm-password)
  (if (and (<= (lispf:cursor-row) 19) (string= confirm-password ""))
      (progn
        (setf (lispf:session-property lispf:*session* :saved-password) new-password)
        (lispf:set-cursor 20 20)
        :stay)
      (progn
        (setf (lispf:session-property lispf:*session* :saved-password) nil)
        (when (string= new-password "")
          (lispf:application-error "Bitte Passwort eingeben"))
        (when (< (length new-password) 6)
          (lispf:application-error "Passwort muss mindestens 6 Zeichen lang sein"))
        (unless (string= new-password confirm-password)
          (lispf:application-error "Passwoerter stimmen nicht ueberein"))
        (let ((user (session-user lispf:*session*)))
          (save-local-password (user-id user) new-password)
          (lispf:set-message :confirmation "Passwort gespeichert"))
        (post-login-screen))))

(lispf:define-key-handler set-password-otp :pf3 ()
  'goodbye)

;;; Change password from menu

(lispf:define-screen-update set-password (old-password new-password confirm-password)
  (setf old-password ""
        new-password ""
        confirm-password ""))

(lispf:define-key-handler set-password :enter (old-password new-password confirm-password)
  (cond
    ;; Cursor on old-password row → move to new-password
    ((and (<= (lispf:cursor-row) 7) (string= new-password ""))
     (lispf:set-cursor 9 20)
     :stay)
    ;; Cursor on new-password row → move to confirm-password
    ((and (<= (lispf:cursor-row) 9) (string= confirm-password ""))
     (lispf:set-cursor 10 20)
     :stay)
    ;; Submit
    (t
     (when (string= old-password "")
       (lispf:application-error "Bitte altes Passwort eingeben"))
     (let* ((user (session-user lispf:*session*))
            (username (user-username user)))
       (unless (or (authenticate-local username old-password)
                   (when (env "VERON_AUTH_DB_HOST" nil)
                     (woltlab-login:authenticate-user username old-password
                                           :host (env "VERON_AUTH_DB_HOST")
                                           :port (parse-integer (env "VERON_AUTH_DB_PORT"))
                                           :database (env "VERON_AUTH_DB_NAME")
                                           :user (env "VERON_AUTH_DB_USER")
                                           :db-password (env "VERON_AUTH_DB_PASSWORD"))))
         (lispf:application-error "Altes Passwort ist falsch")))
     (when (string= new-password "")
       (lispf:application-error "Bitte neues Passwort eingeben"))
     (when (< (length new-password) 6)
       (lispf:application-error "Passwort muss mindestens 6 Zeichen lang sein"))
     (unless (string= new-password confirm-password)
       (lispf:application-error "Passwoerter stimmen nicht ueberein"))
     (let ((user (session-user lispf:*session*)))
       (save-local-password (user-id user) new-password)
       (lispf:set-message :confirmation "Passwort gespeichert"))
     'main)))

(lispf:define-key-handler set-password :pf3 ()
  :back)

;;; Logout

(defun confirm-logout ()
  "Show confirmation dialog and log out if confirmed."
  (lispf:request-confirmation
   "Wirklich abmelden?"
   (lambda ()
     (when (typep lispf:*session* 'authenticated-session)
       (let ((user (session-user lispf:*session*)))
         (notify :logout "Abmeldung"
                 (format nil "~A hat sich abgemeldet" (user-username user))
                 :originator-user-id (user-id user))))
     (if (session-disconnect-p)
         'goodbye
         (signal 'lispf:session-reset)))))

(lispf:define-command *veron-app* (logout)
    (:aliases (logoff exit bye ende quit)
     :doc "Abmelden")
  (confirm-logout))

(defun help-argument (command)
  "Extract the argument after the first space in COMMAND, or NIL if bare command."
  (let ((space (position #\Space command)))
    (when space
      (let ((arg (string-trim '(#\Space) (subseq command (1+ space)))))
        (when (plusp (length arg))
          arg)))))

(defun show-command-help (arg)
  "Display one-liner help for ARG in the error message field."
  (let ((doc (lispf:find-command-doc *veron-app* arg)))
    (if doc
        (lispf:set-message :confirmation "~A - ~A" (string-upcase arg) doc)
        (lispf:set-message :error "~A: Unbekannter Befehl" (string-upcase arg)))))

(defun show-commands-help ()
  "Display a browsable list of all commands in the help viewer."
  (let* ((commands (lispf:collect-all-commands *veron-app*))
         (lines (loop for (name doc aliases) in commands
                      collect (list (format nil "  ~16A ~A~@[ (~{~A~^, ~})~]"
                                            (string-upcase name) (or doc "")
                                            (mapcar #'string-upcase aliases))))))
    (lispf:show-help-page
     (make-instance 'lispf:help-page
                    :title "Verfuegbare Befehle"
                    :lines lines))))

(lispf:define-command *veron-app* (hilfe command)
    (:aliases (help)
     :doc "Hilfe anzeigen")
  (let ((arg (help-argument command)))
    (cond
      ((null arg)
       (lispf:show-help "index")
       :stay)
      ((member arg '("commands" "befehle") :test #'string-equal)
       (show-commands-help)
       :stay)
      (t
       (show-command-help arg)
       :stay))))

;;; Help screen (subapplication handover)

(lispf:define-screen-update hilfe ()
  (lispf:show-help "index")
  :back)

;;; Main screen

(lispf:define-key-handler main :pf3 ()
  (confirm-logout))

;;; Goodbye screen - display for 5 seconds, then disconnect

(lispf:define-key-handler goodbye :pf3 ()
  (if (session-disconnect-p)
      :logoff
      (signal 'lispf:session-reset)))

(defmethod lispf:session-idle-timeout ((app (eql *veron-app*)) session)
  (unless (session-disconnect-p)
    (return-from lispf:session-idle-timeout nil))
  (case (lispf:session-current-screen session)
    (login 60)
    (login-local 60)
    (login-otp 300)
    (set-password-otp 300)
    (goodbye 5)
    (otherwise nil)))

;;; About screen

(lispf:define-key-handler about :pf3 ()
  :back)

;;; Editor settings persistence

(defun apply-editor-settings ()
  "Load editor settings from DB and apply to the current session."
  (let* ((session lispf:*session*)
         (user (session-user session)))
    (when user
      (let ((settings (load-editor-settings (user-id user))))
        (when settings
          (when (getf settings :autoinsert nil)
            (setf (lispf-editor:editor-auto-insert-p session)
                  (getf settings :autoinsert)))
          (when (member :verbose settings)
            (setf (lispf-editor:editor-verbose-p session)
                  (getf settings :verbose))))
        (setf (lispf:session-property session :save-editor-settings)
              (lambda ()
                (save-editor-settings
                 (user-id user)
                 (list :autoinsert (lispf-editor:editor-auto-insert-p session)
                       :verbose (lispf-editor:editor-verbose-p session)))))))))

;;; Notizen (editor)

(lispf:define-screen-update notes ()
  (let* ((user (session-user lispf:*session*))
         (file-id (ensure-notes-file user))
         (path (file-to-disk file-id)))
    (apply-editor-settings)
    (lispf-editor:edit-file path :display-name "Notizen")
    (disk-to-file file-id)
    (cleanup-tmp-file file-id)
    :back))

;;; Dirty-check helpers for edit screens

(defun snapshot-fields (&rest field-names)
  "Save current values of FIELD-NAMES from the session context.
Call from define-screen-enter after populating fields."
  (let ((context (lispf:session-context lispf:*session*)))
    (setf (lispf:session-property lispf:*session* :field-snapshot)
          (loop for name in field-names
                collect (cons name (gethash (string-downcase (string name)) context))))))

(defun fields-dirty-p (&rest field-names)
  "Return T if any of FIELD-NAMES differ from their snapshot values."
  (let ((context (lispf:session-context lispf:*session*))
        (snapshot (lispf:session-property lispf:*session* :field-snapshot)))
    (loop for name in field-names
          for key = (string-downcase (string name))
          for current = (string-trim '(#\Space) (or (gethash key context) ""))
          for saved = (string-trim '(#\Space) (or (cdr (assoc name snapshot :test #'string-equal)) ""))
          thereis (string/= current saved))))

(defun confirm-if-dirty (field-names callback)
  "If any of FIELD-NAMES have changed since snapshot, ask for confirmation.
Otherwise execute CALLBACK immediately."
  (if (apply #'fields-dirty-p field-names)
      (lispf:request-confirmation
       "Aenderungen verwerfen?"
       callback)
      (funcall callback)))

;;; Notification settings screen

(defun field-enabled-p (value)
  "Return T if VALUE is J, Y, or X (case-insensitive). N and space mean disabled."
  (and value
       (let ((trimmed (string-trim '(#\Space) value)))
         (and (plusp (length trimmed))
              (member (char-upcase (char trimmed 0)) '(#\J #\Y #\X))
              t))))

(defun setting-checked (settings event field)
  "Return \"x\" if EVENT has FIELD enabled in SETTINGS alist, empty string otherwise."
  (let ((entry (cdr (assoc event settings))))
    (if (and entry (getf entry field)) "x" "")))

(defparameter *notification-field-names*
  '(topic beep ntfy-guestbook local-guestbook ntfy-login local-login ntfy-logout local-logout))

(lispf:define-screen-enter notifications
    (topic beep ntfy-guestbook local-guestbook ntfy-login local-login ntfy-logout local-logout)
  (let* ((user (session-user lispf:*session*))
         (uid (user-id user))
         (settings (load-notification-settings uid)))
    (setf topic (load-ntfy-topic uid)
          beep (if (load-notification-beep uid) "x" "")
          ntfy-guestbook (setting-checked settings :guestbook :ntfy)
          local-guestbook (setting-checked settings :guestbook :local)
          ntfy-login (setting-checked settings :login :ntfy)
          local-login (setting-checked settings :login :local)
          ntfy-logout (setting-checked settings :logout :ntfy)
          local-logout (setting-checked settings :logout :local)))
  (apply #'snapshot-fields *notification-field-names*))

(lispf:define-key-handler notifications :pf3 ()
  (confirm-if-dirty *notification-field-names* (lambda () :back)))

(lispf:define-key-handler notifications :pf5
    (topic beep ntfy-guestbook local-guestbook ntfy-login local-login ntfy-logout local-logout)
  (let* ((user (session-user lispf:*session*))
         (uid (user-id user))
         (topic-name (string-trim '(#\Space) (or topic "")))
         (has-ntfy (or (field-enabled-p ntfy-guestbook)
                       (field-enabled-p ntfy-login)
                       (field-enabled-p ntfy-logout))))
    (when (and has-ntfy (string= topic-name ""))
      (lispf:application-error "Bitte ntfy-Topic eingeben"))
    (save-ntfy-topic uid topic-name)
    (save-notification-beep uid (field-enabled-p beep))
    (save-notification-setting uid :guestbook
                               (field-enabled-p ntfy-guestbook)
                               (field-enabled-p local-guestbook))
    (save-notification-setting uid :login
                               (field-enabled-p ntfy-login)
                               (field-enabled-p local-login))
    (save-notification-setting uid :logout
                               (field-enabled-p ntfy-logout)
                               (field-enabled-p local-logout))
    (apply #'snapshot-fields *notification-field-names*)
    (lispf:set-message :confirmation "Gespeichert")
    :stay))

(lispf:define-key-handler notifications :pf9
    (ntfy-guestbook local-guestbook ntfy-login local-login ntfy-logout local-logout)
  (let ((any-on (or (field-enabled-p ntfy-guestbook) (field-enabled-p local-guestbook)
                    (field-enabled-p ntfy-login) (field-enabled-p local-login)
                    (field-enabled-p ntfy-logout) (field-enabled-p local-logout))))
    (let ((val (if any-on "" "x")))
      (setf ntfy-guestbook val local-guestbook val
            ntfy-login val local-login val
            ntfy-logout val local-logout val)))
  :stay)

;;; Login log screen

(lispf:define-list-data-getter log (start end)
  (let* ((total (login-log-count))
         (entries (login-log-entries start (- end start))))
    (values (loop for e in entries
                  collect (list :name (getf e :name)
                                :login-at (let ((ts (getf e :login-at)))
                                            (if ts (format-datetime ts) ""))
                                :logout-at (let ((ts (getf e :logout-at)))
                                             (if ts (format-datetime ts) ""))
                                :terminal-type (or (getf e :terminal-type) "")
                                :tls (if (getf e :tls) "Ja" "")))
            total)))

;;; Deployment logging

(defvar *last-deploy-hash* nil
  "Git commit hash of the last deployment, used to detect new deploys.")

(defun git-short-hash ()
  "Return the short git commit hash, or NIL."
  (ignore-errors
    (string-trim '(#\Newline #\Return #\Space)
                 (uiop:run-program '("git" "rev-parse" "--short" "HEAD")
                                   :output :string
                                   :directory (asdf:system-source-directory :veron)))))

(defun git-commit-titles-since (old-hash)
  "Return a list of commit title strings from OLD-HASH to HEAD."
  (ignore-errors
    (let ((output (uiop:run-program
                   (list "git" "log" "--pretty=format:%s"
                         (format nil "~A..HEAD" old-hash))
                   :output :string
                   :directory (asdf:system-source-directory :veron))))
      (when (and output (plusp (length output)))
        (uiop:split-string output :separator '(#\Newline))))))

(defun format-changelog-entry (hash titles)
  "Format a changelog entry block with timestamp heading, hash, and indented titles."
  (with-output-to-string (s)
    (multiple-value-bind (sec min hour day month year)
        (decode-display-time (get-universal-time))
      (declare (ignore sec))
      (format s "=== Deployment, git hash ~A, ~2,'0D.~2,'0D.~4D ~2,'0D:~2,'0D ===~%"
              hash day month year hour min))
    (dolist (title titles)
      (format s "    ~A~%" title))
    (terpri s)))

(defun changelog-contains-hash-p (hash)
  "Return T if the changelog already contains an entry for HASH."
  (let ((text (load-changelog-text)))
    (or (search (format nil "git hash ~A" hash) text)
        (search (format nil "(~A)" hash) text))))

(defun append-changelog-deployment ()
  "If there are new commits since last deploy, prepend a changelog entry.
Returns T if an entry was added."
  (let ((current-hash (git-short-hash)))
    (unless current-hash
      (return-from append-changelog-deployment nil))
    (when (changelog-contains-hash-p current-hash)
      (setf *last-deploy-hash* current-hash)
      (return-from append-changelog-deployment nil))
    (let ((titles (if *last-deploy-hash*
                      (git-commit-titles-since *last-deploy-hash*)
                      (ignore-errors
                        (let ((output (uiop:run-program
                                       '("git" "log" "-1" "--pretty=format:%s")
                                       :output :string
                                       :directory (asdf:system-source-directory :veron))))
                          (when (and output (plusp (length output)))
                            (list (string-trim '(#\Newline #\Return #\Space) output))))))))
      (when titles
        (let* ((new-block (format-changelog-entry current-hash titles))
               (existing (load-changelog-text))
               (combined (concatenate 'string new-block existing)))
          (save-changelog-text combined))))
    (setf *last-deploy-hash* current-hash)
    t))

(defun log-deployment ()
  "Log a deployment and update the changelog."
  (let ((hash (git-short-hash)))
    (lispf:log-message :info "deployed ~A" (or hash "unknown")))
  (ensure-changelog-file)
  (when (append-changelog-deployment)
    (lispf:log-message :info "changelog updated")))

;;; Login redirect

(defun post-login-screen ()
  "Return the screen to navigate to after login.
If the changelog has unread entries, go to changelog; otherwise main."
  (let ((user (session-user lispf:*session*)))
    (cond
      ((and user (changelog-unread-p (user-id user)))
       (setf (lispf:session-property lispf:*session* :changelog-post-login) t)
       'changelog)
      (t 'main))))

;;; Update cycle hook — notification delivery to error line and MSG indicator

(defmethod lispf:update-cycle-hook ((app (eql *veron-app*)))
  (deliver-notification-to-error-line))

;;; Message cleared hook — mark notifications as read when user dismisses

(defmethod lispf:message-cleared ((app (eql *veron-app*)) message)
  (when (and (eq (getf message :type) :notification)
             (typep lispf:*session* 'authenticated-session))
    (let ((displayed-id (lispf:session-property lispf:*session* :notification-displayed-id)))
      (when displayed-id
        (mark-inbox-seen (user-id (session-user lispf:*session*)) displayed-id)
        (setf (lispf:session-property lispf:*session* :notification-displayed-id) nil)
        (update-notification-indicator)))))

;;; Server entry point

(defun deploy ()
  "Run all deployment steps: migrations, chat, screens, orphan cleanup, changelog."
  (lispf:log-message :info "running migrations")
  (initialize-db)
  (lispf:log-message :info "loading chat from DB")
  (load-chat-from-db)
  (lispf:log-message :info "loading message catalog")
  (lispf:set-message-catalog
   *veron-app*
   (merge-pathnames #P"i18n/de.lisp"
                    (asdf:system-source-directory :lispf)))
  (lispf:log-message :info "refreshing screens and menus")
  (let ((lispf:*application* *veron-app*))
    (lispf:reload-all-screens)
    (lispf:load-application-menus *veron-app*))
  (close-orphaned-chat-sessions)
  (lispf:log-message :info "starting notification delivery thread")
  (start-delivery-thread)
  (log-deployment)
  (lispf:log-message :info "deployment complete"))

(defun start (&key (port 3270) (host "127.0.0.1")
                    tls-port tls-certificate-file tls-key-file key-password (starttls t))
  "Start the VERON application on PORT.
When TLS-CERTIFICATE-FILE and TLS-KEY-FILE are provided, TLS is available.
TLS-PORT enables a dedicated TLS listener. STARTTLS (default T) offers
STARTTLS negotiation on the plain port."
  (when (or tls-certificate-file tls-key-file)
    (unless (and tls-certificate-file tls-key-file)
      (error "Both :tls-certificate-file and :tls-key-file must be provided"))
    (unless (probe-file tls-certificate-file)
      (error "TLS certificate file not found: ~A" tls-certificate-file))
    (unless (probe-file tls-key-file)
      (error "TLS key file not found: ~A" tls-key-file)))
  (deploy)
  (lispf:start-application *veron-app* :port port :host host
                           :tls-port tls-port
                           :certificate-file tls-certificate-file
                           :key-file tls-key-file
                           :key-password key-password
                           :starttls starttls))

(defun maybe-start-swank ()
  "Start a Swank server if SWANK_PORT is set in the environment.
Overrides quit-lisp so that ,q in SLIME closes the connection
instead of killing the server process."
  (when-let (port-string (env "SWANK_PORT" nil))
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
           :tls-certificate-file cert
           :tls-key-file key
           :key-password key-pw
           :starttls starttls)))


(defun reload ()
  "Hot-reload VERON code, run migrations, and refresh caches.
Called via Swank during deployment. Existing sessions continue running."
  (lispf:log-message :info "hot-reload starting")
  (dolist (sys '(:veron :lispf :lispf-edit :woltlab-login))
    (asdf:clear-system sys))
  (lispf:log-message :info "reloading code")
  (handler-bind ((style-warning #'muffle-warning))
    (funcall (find-symbol "QUICKLOAD" :ql) :veron))
  ;; After quickload, deploy is the new version
  (funcall (find-symbol "DEPLOY" :veron))
  (lispf:log-message :info "hot-reload complete")
  :ok)

