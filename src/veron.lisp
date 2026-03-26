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
   (otp-username :initform nil :accessor session-otp-username)
   (otp-email-masked :initform nil :accessor session-otp-email-masked)
   (otp-attempts :initform 0 :accessor session-otp-attempts)))

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
  (let ((login-id (session-login-id session))
        (user (session-user session)))
    (when login-id
      (record-logout login-id))
    (when (and user (lspf:session-property session :chat-entered)
               (not (lspf:session-property session :chat-leaving)))
      (let ((channel-id (default-channel-id)))
        (when channel-id
          (add-chat-notification channel-id user
                                 "--- ~A hat den Chat verlassen"
                                 (user-username user)))))
    (when user
      (lspf:log-message :info "logout user=~A" (user-username user)))))

;;; Application customization

(defmethod lspf:unknown-key-message ((app (eql *veron-app*)) key-name)
  (format nil "~A: Unbekannte Taste" key-name))

(defmethod lspf:unknown-command-message ((app (eql *veron-app*)) command)
  (format nil "~A: Unbekannter Befehl" (string-upcase command)))

(defmethod lspf:default-command-label ((app (eql *veron-app*)))
  "Befehl   ==>")

(defmethod lspf:menu-command-label ((app (eql *veron-app*)))
  "Auswahl ==>")

(defmethod lspf:paging-labels ((app (eql *veron-app*)))
  (values "Vor." "Naech."))

(defmethod lspf:menu-key-labels ((app (eql *veron-app*)) &optional menu-name)
  (values "Auswahl" (if (equal menu-name "main") "Abmelden" "Zurueck")))

(defmethod lspf:session-authenticated-p ((app (eql *veron-app*)) session)
  (not (null (session-user session))))

(defmethod lspf:anonymous-access-denied-message ((app (eql *veron-app*)))
  "Anmeldung erforderlich")

(defmethod lspf:session-user-roles ((app (eql *veron-app*)) session)
  (let ((user (session-user session)))
    (when user (effective-roles user))))

(defmethod lspf:role-access-denied-message ((app (eql *veron-app*)))
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
      (format t "~&;;; VERON: closed ~D orphaned session~:P~%" (length users))
      (lspf:log-message :info "Closed ~D orphaned session~:P" (length users)))))

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
        (setf password-label "Passwort:")
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
       (lspf:set-cursor 20 20)
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
         (post-login-screen)))
      ;; Non-TLS: user has local password
      ((has-local-password-p username)
       (setf (session-otp-username lspf:*session*) username)
       'login-local)
      ;; Non-TLS: WoltLab user without local password, send OTP
      ((lookup-woltlab-email username)
       (prepare-otp-login username)
       'login-otp)
      ;; Unknown user
      (t
       (lspf:application-error "Benutzer nicht gefunden")))))

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

(lspf:define-key-handler login-local :pf5 ()
  (let ((username (session-otp-username lspf:*session*)))
    (unless (lookup-woltlab-email username)
      (lspf:application-error "Passwort-Reset nicht moeglich"))
    (prepare-otp-login username)
    'login-otp))

;;; Login with OTP screen

(lspf:define-screen-update login-otp (username otp-code)
  (let ((session lspf:*session*))
    (setf username (or (session-otp-username session) "")
          otp-code "")
    (when (not (gethash "errormsg" (lspf:session-context session)))
      (lspf:set-field-attribute "errormsg" :color cl3270:+yellow+)
      (let ((minutes-ago (lspf:session-property session :otp-minutes-ago))
            (email (or (session-otp-email-masked session) "")))
        (setf (gethash "errormsg" (lspf:session-context session))
              (if minutes-ago
                  (format nil "Einmalpasswort gesendet vor ~D Min. an: ~A"
                          minutes-ago email)
                  (format nil "Einmalpasswort gesendet an: ~A" email)))))))

(lspf:define-key-handler login-otp :enter (otp-code)
  (verify-otp (session-otp-username lspf:*session*) otp-code))

(lspf:define-key-handler login-otp :pf3 ()
  'login)

;;; Set password after OTP login (overlay)

(lspf:define-screen-update set-password-otp (new-password confirm-password)
  (let ((saved (lspf:session-property lspf:*session* :saved-password)))
    (setf new-password (or saved "")
          confirm-password ""))
  (when (not (gethash "errormsg" (lspf:session-context lspf:*session*)))
    (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
          "Bitte lokales Passwort setzen")))

(lspf:define-key-handler set-password-otp :enter (new-password confirm-password)
  (if (and (<= (lspf:cursor-row) 19) (string= confirm-password ""))
      (progn
        (setf (lspf:session-property lspf:*session* :saved-password) new-password)
        (lspf:set-cursor 20 20)
        :stay)
      (progn
        (setf (lspf:session-property lspf:*session* :saved-password) nil)
        (when (string= new-password "")
          (lspf:application-error "Bitte Passwort eingeben"))
        (when (< (length new-password) 6)
          (lspf:application-error "Passwort muss mindestens 6 Zeichen lang sein"))
        (unless (string= new-password confirm-password)
          (lspf:application-error "Passwoerter stimmen nicht ueberein"))
        (let ((user (session-user lspf:*session*)))
          (save-local-password (user-id user) new-password)
          (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
                "Passwort gespeichert"))
        (post-login-screen))))

(lspf:define-key-handler set-password-otp :pf3 ()
  'goodbye)

;;; Change password from menu

(lspf:define-screen-update set-password (old-password new-password confirm-password)
  (setf old-password ""
        new-password ""
        confirm-password ""))

(lspf:define-key-handler set-password :enter (old-password new-password confirm-password)
  (cond
    ;; Cursor on old-password row → move to new-password
    ((and (<= (lspf:cursor-row) 7) (string= new-password ""))
     (lspf:set-cursor 9 20)
     :stay)
    ;; Cursor on new-password row → move to confirm-password
    ((and (<= (lspf:cursor-row) 9) (string= confirm-password ""))
     (lspf:set-cursor 10 20)
     :stay)
    ;; Submit
    (t
     (when (string= old-password "")
       (lspf:application-error "Bitte altes Passwort eingeben"))
     (let* ((user (session-user lspf:*session*))
            (username (user-username user)))
       (unless (or (authenticate-local username old-password)
                   (when (env "VERON_AUTH_DB_HOST" nil)
                     (wl:authenticate-user username old-password
                                           :host (env "VERON_AUTH_DB_HOST")
                                           :port (parse-integer (env "VERON_AUTH_DB_PORT"))
                                           :database (env "VERON_AUTH_DB_NAME")
                                           :user (env "VERON_AUTH_DB_USER")
                                           :db-password (env "VERON_AUTH_DB_PASSWORD"))))
         (lspf:application-error "Altes Passwort ist falsch")))
     (when (string= new-password "")
       (lspf:application-error "Bitte neues Passwort eingeben"))
     (when (< (length new-password) 6)
       (lspf:application-error "Passwort muss mindestens 6 Zeichen lang sein"))
     (unless (string= new-password confirm-password)
       (lspf:application-error "Passwoerter stimmen nicht ueberein"))
     (let ((user (session-user lspf:*session*)))
       (save-local-password (user-id user) new-password)
       (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
             "Passwort gespeichert"))
     'main)))

(lspf:define-key-handler set-password :pf3 ()
  :back)

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
    (set-password-otp 300)
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
  "Log a deployment to console and LISPF log, and update the changelog."
  (let ((hash (git-short-hash)))
    (format t "~&;;; VERON: deployed ~A~%" (or hash "unknown"))
    (lspf:log-message :info "Deployed ~A" (or hash "unknown")))
  (ensure-changelog-file)
  (when (append-changelog-deployment)
    (format t "~&;;; VERON: changelog updated~%")
    (lspf:log-message :info "Changelog updated")))

;;; Login redirect

(defun post-login-screen ()
  "Return the screen to navigate to after login.
If the changelog has unread entries, go to changelog; otherwise main."
  (let ((user (session-user lspf:*session*)))
    (cond
      ((and user (changelog-unread-p (user-id user)))
       (setf (lspf:session-property lspf:*session* :changelog-post-login) t)
       'changelog)
      (t 'main))))

;;; Server entry point

(defun start (&key (port 3270) (host "127.0.0.1")
                    tls-port certificate-file key-file key-password (starttls t))
  "Start the VERON application on PORT.
When CERTIFICATE-FILE and KEY-FILE are provided, TLS is available.
TLS-PORT enables a dedicated TLS listener. STARTTLS (default T) offers
STARTTLS negotiation on the plain port."
  (initialize-db)
  (load-chat-from-db)
  (close-orphaned-chat-sessions)
  (log-deployment)
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
  (force-output)
  (handler-bind ((style-warning #'muffle-warning))
    (funcall (find-symbol "QUICKLOAD" :ql) :veron))
  (format t "~&;;; VERON: code reload done, running post-code steps~%")
  (force-output)
  ;; After quickload, reload-post-code is the new version
  (reload-post-code)
  (format t "~&;;; VERON: hot-reload complete~%")
  (force-output)
  :ok)

(defun reload-post-code ()
  "Post-code-reload steps: migrations, chat, screen cache, menus."
  (format t "~&;;; VERON: running migrations~%")
  (force-output)
  (initialize-db)
  (format t "~&;;; VERON: reloading chat from DB~%")
  (force-output)
  (bt:with-lock-held (*chat-lock*)
    (load-chat-from-db))
  (format t "~&;;; VERON: refreshing screens and menus~%")
  (force-output)
  (let ((lspf:*application* *veron-app*))
    (lspf:reload-all-screens)
    (lspf:load-application-menus *veron-app*))
  (format t "~&;;; VERON: logging deployment~%")
  (force-output)
  (log-deployment)
  (format t "~&;;; VERON: reload-post-code done~%")
  (force-output))
