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

;;; Session class

(defclass veron-session (lispf-editor:editor-session)
  ((user :initform nil :accessor session-user)
   (term-type :initform "" :accessor session-term-type)
   (connect-time :initform (get-universal-time) :reader session-connect-time)
   (login-id :initform nil :accessor session-login-id)
   (otp-username :initform nil :accessor session-otp-username)
   (otp-attempts :initform 0 :accessor session-otp-attempts)
   (password-reset-sent :initform nil :accessor session-password-reset-sent)))

;;; Application definition

(lispf:define-application *veron-app*
  :name "veron"
  :title "VERON"
  :entry-screen login
  :screen-directory (merge-pathnames
                     #P"screens/"
                     (asdf:system-source-directory :veron))
  :session-class 'veron-session)

;;; Session cleanup - called for all disconnection paths

(defmethod lispf:session-cleanup ((app (eql *veron-app*)) session)
  (let ((login-id (session-login-id session))
        (user (session-user session)))
    (when login-id
      (record-logout login-id))
    (when (and user (lispf:session-property session :chat-entered)
               (not (lispf:session-property session :chat-leaving)))
      (let ((channel-id (default-channel-id)))
        (when channel-id
          (add-chat-notification channel-id user
                                 "--- ~A hat den Chat verlassen"
                                 (user-username user)))))
    (when user
      (lispf:log-message :info "logout user=~A" (user-username user)))))

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
  (not (null (session-user session))))

(defmethod lispf:anonymous-access-denied-message ((app (eql *veron-app*)))
  "Anmeldung erforderlich")

(defmethod lispf:session-user-roles ((app (eql *veron-app*)) session)
  (let ((user (session-user session)))
    (when user (effective-roles user))))

(defmethod lispf:role-access-denied-message ((app (eql *veron-app*)))
  "Keine Berechtigung")


;;; Orphaned session cleanup

(defun close-orphaned-chat-sessions ()
  "Close open login sessions from previous server instances and add chat disconnect notifications."
  (let ((users (close-orphaned-sessions)))
    (when users
      (let ((channel-id (default-channel-id)))
        (when channel-id
          (dolist (user-pair users)
            (destructuring-bind (user-id username) user-pair
              (let* ((msg-text (format nil "--- ~A wurde getrennt (Neustart)" username))
                     (db-id (insert-chat-message channel-id user-id username
                                                 msg-text "notification"))
                     (msg (list :id db-id
                                :type :notification
                                :message msg-text
                                :created-at (get-universal-time))))
                (bt:with-lock-held (*chat-lock*)
                  (let ((buf (ensure-channel-buffer channel-id)))
                    (vector-push-extend msg buf))
                  (setf *chat-id-counter* (max *chat-id-counter* db-id))))))))
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
  (when (and lispf:*device-info* (string= "" (session-term-type lispf:*session*)))
    (setf (session-term-type lispf:*session*) (cl3270::term-type lispf:*device-info*)))
  (setf password "")
  (cond
    ((lispf:session-tls-p lispf:*session*)
     (setf password-label "Passwort:")
     (lispf:show-key :pf2 "Passwort vergessen"))
    (t
     (setf password-label "")
     (lispf:set-field-attribute "password" :write nil)
     (unless (gethash "%errormsg" (lispf:session-context lispf:*session*))
       (setf (gethash "%errormsg" (lispf:session-context lispf:*session*))
             "Warnung: Verbindung ist nicht verschluesselt!")))))

(lispf:define-key-handler login :enter (username password)
  (when (string= username "")
    (lispf:application-error "Bitte Benutzername eingeben"))
  (let ((tls-p (lispf:session-tls-p lispf:*session*)))
    (cond
      ;; TLS: cursor on username row → move to password field
      ((and tls-p (<= (lispf:cursor-row) 19) (string= password ""))
       (lispf:next-input-field))
      ;; TLS: standard WoltLab login, also accepts valid OTP as password
      (tls-p
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
         (complete-login result)
         (if (getf result :otp-login)
             'set-password-otp
             (post-login-screen))))
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
    (when (not (gethash "%errormsg" (lispf:session-context session)))
      (lispf:set-field-attribute "%errormsg" :color cl3270:+yellow+)
      (setf (gethash "%errormsg" (lispf:session-context session))
            "Falls ein Konto existiert, wurde eine E-Mail gesendet"))))

(lispf:define-key-handler login-otp :enter (otp-code)
  (verify-otp (session-otp-username lispf:*session*) otp-code))

(lispf:define-key-handler login-otp :pf3 ()
  'login)

;;; Set password after OTP login (overlay)

(lispf:define-screen-update set-password-otp (new-password confirm-password)
  (let ((saved (lispf:session-property lispf:*session* :saved-password)))
    (setf new-password (or saved "")
          confirm-password ""))
  (when (not (gethash "%errormsg" (lispf:session-context lispf:*session*)))
    (setf (gethash "%errormsg" (lispf:session-context lispf:*session*))
          "Bitte lokales Passwort setzen")))

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
          (setf (gethash "%errormsg" (lispf:session-context lispf:*session*))
                "Passwort gespeichert"))
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
       (setf (gethash "%errormsg" (lispf:session-context lispf:*session*))
             "Passwort gespeichert"))
     'main)))

(lispf:define-key-handler set-password :pf3 ()
  :back)

;;; Logout

(defun confirm-logout ()
  "Show confirmation dialog and log out if confirmed."
  (lispf:request-confirmation
   "Wirklich abmelden?"
   (lambda ()
     (let ((user (session-user lispf:*session*)))
       (when user
         (notify :logout "Abmeldung"
                 (format nil "~A hat sich abgemeldet" (user-username user)))))
     'goodbye)))

(defvar *logout-commands* '("logout" "logoff" "exit" "bye" "ende" "quit")
  "Commands that trigger the logout confirmation dialog.")

(defvar *help-commands* '("hilfe" "help")
  "Commands that open the help viewer.")

(defmethod lispf:process-command ((app (eql *veron-app*)) (command string))
  (cond
    ((member command *logout-commands* :test #'string-equal)
     (confirm-logout))
    ((member command *help-commands* :test #'string-equal)
     (lispf:show-help "index")
     :stay)
    (t (call-next-method))))

;;; Help screen (subapplication handover)

(lispf:define-screen-update hilfe ()
  (lispf:show-help "index")
  :back)

;;; Main screen

(lispf:define-key-handler main :pf3 ()
  (confirm-logout))

;;; Goodbye screen - display for 5 seconds, then disconnect

(lispf:define-key-handler goodbye :pf3 ()
  :logoff)

(defmethod lispf:session-idle-timeout ((app (eql *veron-app*)) session)
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

;;; Notification settings screen

(defun field-enabled-p (value)
  "Return T if a checkbox field value represents 'enabled' (any non-blank char)."
  (and value (plusp (length (string-trim '(#\Space) value)))))

(defun event-checked (events event)
  "Return \"x\" if EVENT keyword is in the EVENTS list, empty string otherwise."
  (if (member event events) "x" ""))

(lispf:define-screen-update notifications
    (topic evt-guestbook evt-login evt-logout)
  (let* ((user (session-user lispf:*session*))
         (subs (user-subscriptions (user-id user)))
         (sub (first subs)))
    (when sub
      (let ((events (getf sub :events)))
        (setf topic (getf sub :topic)
              evt-guestbook (event-checked events :guestbook)
              evt-login (event-checked events :login)
              evt-logout (event-checked events :logout))))))

(lispf:define-key-handler notifications :pf5
    (topic evt-guestbook evt-login evt-logout)
  (let* ((user (session-user lispf:*session*))
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
       (setf (gethash "%errormsg" (lispf:session-context lispf:*session*))
             "Benachrichtigungen deaktiviert"))
      ((string= topic-name "")
       (lispf:application-error "Bitte ntfy-Topic eingeben"))
      ((null events)
       (setf (gethash "%errormsg" (lispf:session-context lispf:*session*))
             "Benachrichtigungen deaktiviert"))
      (t
       (subscribe (user-id user) topic-name (nreverse events))
       (setf (gethash "%errormsg" (lispf:session-context lispf:*session*))
             "Gespeichert")))
    :stay))

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

;;; Server entry point

(defun deploy ()
  "Run all deployment steps: migrations, chat, screens, orphan cleanup, changelog."
  (lispf:log-message :info "running migrations")
  (initialize-db)
  (lispf:log-message :info "loading chat from DB")
  (bt:with-lock-held (*chat-lock*)
    (load-chat-from-db))
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
  (log-deployment)
  (lispf:log-message :info "deployment complete"))

(defun start (&key (port 3270) (host "127.0.0.1")
                    tls-port certificate-file key-file key-password (starttls t))
  "Start the VERON application on PORT.
When CERTIFICATE-FILE and KEY-FILE are provided, TLS is available.
TLS-PORT enables a dedicated TLS listener. STARTTLS (default T) offers
STARTTLS negotiation on the plain port."
  (deploy)
  (lispf:start-application *veron-app* :port port :host host
                           :tls-port tls-port
                           :certificate-file certificate-file
                           :key-file key-file
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
           :certificate-file cert
           :key-file key
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

