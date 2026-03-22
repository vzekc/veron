;;; -*- Mode: Lisp -*-

;;; VERON - V.z.E.k.C. Electronic Retro Online Network
;;; Main application using LISPF framework with WoltLab authentication.

(in-package #:veron)

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
   (login-id :initform nil :accessor session-login-id)))

;;; Application definition

(lspf:define-application *veron-app*
  :title "VERON"
  :entry-screen login
  :screen-directory (merge-pathnames
                     #P"screens/"
                     (asdf:system-source-directory :veron))
  :session-class 'veron-session)

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

(lspf:define-screen-update login ()
  (when (and lspf:*device-info* (string= "" (session-term-type lspf:*session*)))
    (setf (session-term-type lspf:*session*) (cl3270::term-type lspf:*device-info*))))

(lspf:define-key-handler login :enter (username password)
  (when (or (string= username "") (string= password ""))
    (lspf:application-error "Bitte Benutzername und Passwort eingeben"))
  (let ((result (wl:authenticate-user username password
                                      :host (env "VERON_AUTH_DB_HOST")
                                      :port (parse-integer (env "VERON_AUTH_DB_PORT"))
                                      :database (env "VERON_AUTH_DB_NAME")
                                      :user (env "VERON_AUTH_DB_USER")
                                      :db-password (env "VERON_AUTH_DB_PASSWORD"))))
    (unless result
      (lspf:application-error "Ungueltiger Benutzername oder Passwort"))
    (let ((user (make-user result)))
      (setf (session-user lspf:*session*) user)
      (ensure-db-user user)
      (setf (session-login-id lspf:*session*)
            (record-login user :terminal-type (session-term-type lspf:*session*)))
      (notify :login "Anmeldung"
              (format nil "~A hat sich angemeldet" (user-username user))))
    'main))

(lspf:define-key-handler login :pf3 ()
  :logoff)

;;; Main screen

(lspf:define-key-handler main :pf3 ()
  'abmelden)

;;; Logout confirmation

(lspf:define-key-handler abmelden :pf5 ()
  (let ((user (session-user lspf:*session*)))
    (record-logout (session-login-id lspf:*session*))
    (when user
      (notify :logout "Abmeldung"
              (format nil "~A hat sich abgemeldet" (user-username user)))))
  :logoff)

;;; About screen

(lspf:define-key-handler about :pf3 ()
  :back)

;;; Who's online screen

(defun format-session-line (index session now)
  "Format a single session line for the who display."
  (let* ((user (session-user session))
         (active-app (lspf:session-active-application session))
         (username (if user (user-username user) "(login)"))
         (app-name (if active-app
                       (or (lspf:application-title active-app)
                           (lspf:application-name active-app))
                       ""))
         (screen (string-downcase
                  (string (lspf:session-current-screen session))))
         (connected (format-duration (- now (session-connect-time session)))))
    (format nil "~3D. ~16A ~8A ~15A ~A"
            (1+ index) username app-name screen connected)))

(lspf:define-dynamic-area-updater who sessions ()
  (let ((now (get-universal-time))
        (lines '())
        (index 0))
    (bt:with-lock-held ((lspf::application-sessions-lock lspf:*application*))
      (dolist (s (lspf::application-sessions lspf:*application*))
        (let ((line (format-session-line index s now))
              (current-p (eq s lspf:*session*)))
          (push (if current-p
                    (list :content line :color cl3270:+white+ :intense t)
                    line)
                lines))
        (incf index)))
    (nreverse lines)))

;;; Guestbook screens

(lspf:define-list-data-getter gaestebuch (start end)
  (let* ((total (guestbook-count))
         (entries (guestbook-entries start (- end start))))
    (values (loop for e in entries
                  collect (list :author (getf e :author)
                                :date (let ((ts (getf e :created-at)))
                                        (if ts (format-datetime ts) ""))
                                :preview (substitute #\Space #\Newline
                                                     (getf e :message))))
            total)))

(lspf:define-key-handler gaestebuch :enter ()
  (let ((index (lspf:selected-list-index)))
    (when index
      (let* ((entries (guestbook-entries index 1))
             (entry (first entries)))
        (when entry
          (setf (lspf:session-property lspf:*session* :browse-entry) entry)
          (setf (lspf:session-property lspf:*session* :browse-index) index)
          'gaestebuch-eintrag)))))

(lspf:define-screen-update gaestebuch-eintrag (author date message)
  (let ((entry (lspf:session-property lspf:*session* :browse-entry)))
    (when entry
      (setf author (getf entry :author)
            date (let ((ts (getf entry :created-at)))
                   (if ts (format-datetime ts) ""))
            message (getf entry :message))))
  (let ((user (session-user lspf:*session*)))
    (when (admin-p user)
      (lspf:show-key :pf5 "Loeschen")))
  (let ((index (lspf:session-property lspf:*session* :browse-index))
        (total (guestbook-count)))
    (when (and index (> index 0))
      (lspf:show-key :pf7 "Vor."))
    (when (and index (< index (1- total)))
      (lspf:show-key :pf8 "Naech."))))

(defun browse-guestbook-entry (direction)
  "Navigate to prev (-1) or next (+1) guestbook entry. Returns :stay."
  (let* ((index (lspf:session-property lspf:*session* :browse-index))
         (new-index (when index (+ index direction)))
         (entries (when new-index (guestbook-entries new-index 1)))
         (entry (first entries)))
    (when entry
      (setf (lspf:session-property lspf:*session* :browse-entry) entry)
      (setf (lspf:session-property lspf:*session* :browse-index) new-index)))
  :stay)

(lspf:define-key-handler gaestebuch-eintrag :pf7 ()
  (browse-guestbook-entry -1))

(lspf:define-key-handler gaestebuch-eintrag :pf8 ()
  (browse-guestbook-entry 1))

(lspf:define-key-handler gaestebuch-eintrag :pf5 ()
  (let ((user (session-user lspf:*session*)))
    (unless (admin-p user)
      (lspf:application-error "Keine Berechtigung"))
    (let ((entry (lspf:session-property lspf:*session* :browse-entry)))
      (when entry
        (setf (lspf:session-property lspf:*session* :confirm-delete)
              (getf entry :id))
        'gaestebuch-loeschen))))

;;; Guestbook delete confirmation

(lspf:define-key-handler gaestebuch-loeschen :pf5 ()
  (let ((entry-id (lspf:session-property lspf:*session* :confirm-delete)))
    (when entry-id
      (let ((index (lspf:session-property lspf:*session* :browse-index)))
        (delete-guestbook-entry entry-id)
        (setf (lspf:session-property lspf:*session* :confirm-delete) nil)
        ;; Try to show the next entry (which now occupies the same index)
        (let* ((entries (guestbook-entries index 1))
               (entry (first entries)))
          (cond
            (entry
             ;; Next entry exists at same index
             (setf (lspf:session-property lspf:*session* :browse-entry) entry)
             (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
                   "Eintrag geloescht")
             :back)  ; back to gaestebuch-eintrag
            (t
             ;; No more entries, return to list
             (setf (lspf:session-property lspf:*session* :browse-entry) nil)
             (pop (lspf:session-screen-stack lspf:*session*))
             (setf (lspf:list-offset lspf:*session* 'gaestebuch) 0)
             (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
                   "Eintrag geloescht")
             :back)))))))

;;; Guestbook new entry

(lspf:define-screen-update gaestebuch-neu (author message)
  (let ((user (session-user lspf:*session*)))
    (when (and user (string= author ""))
      (setf author (user-username user)))
    (when user
      (lspf:set-field-attribute "author" :write nil :intense t)))
  ;; Restore message from session property when returning from confirmation
  (let ((saved-message (lspf:session-property lspf:*session* :new-entry-message)))
    (when (and saved-message (string= message ""))
      (setf message saved-message))))

(lspf:define-key-handler gaestebuch-neu :pf5 (author message)
  (let ((user (session-user lspf:*session*)))
    (when (string= message "")
      (lspf:application-error "Bitte Nachricht eingeben"))
    (let ((name (if user
                    (user-username user)
                    (if (string= author "")
                        (lspf:application-error "Bitte Name eingeben")
                        (format nil "~A (Gast)" author)))))
      (setf (lspf:session-property lspf:*session* :new-entry-author) name)
      (setf (lspf:session-property lspf:*session* :new-entry-message) message))
    'gaestebuch-pruefen))

;;; Guestbook save confirmation

(lspf:define-screen-update gaestebuch-pruefen (author message)
  (setf author (lspf:session-property lspf:*session* :new-entry-author))
  (setf message (lspf:session-property lspf:*session* :new-entry-message)))

(lspf:define-key-handler gaestebuch-pruefen :pf5 ()
  (let ((author (lspf:session-property lspf:*session* :new-entry-author))
        (message (lspf:session-property lspf:*session* :new-entry-message)))
    (when (and author message)
      (add-guestbook-entry author message)
      (notify :guestbook "Neuer Gaestebucheintrag"
              (format nil "~A: ~A" author
                      (subseq message 0 (min 100 (length message)))))
      (setf (lspf:session-property lspf:*session* :new-entry-author) nil)
      (setf (lspf:session-property lspf:*session* :new-entry-message) nil)
      ;; Reset list offset so the new entry is visible at the top
      (setf (lspf:list-offset lspf:*session* 'gaestebuch) 0)
      ;; Set confirmation message for the guestbook list
      (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
            "Eintrag gespeichert")))
  ;; Pop back past gaestebuch-neu to the guestbook list
  (pop (lspf:session-screen-stack lspf:*session*))
  :back)

;;; Notizen (editor demo)

(lspf:define-screen-update notizen ()
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

(lspf:define-screen-update benachrichtigungen
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

(lspf:define-key-handler benachrichtigungen :pf5
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

;;; Chat screen

(defvar *chat-help-text*
  '("Chat-Hilfe:"
    ""
    "  Nachrichten eingeben und mit ENTER senden."
    "  Alle oeffentlichen Nachrichten werden in der"
    "  Datenbank gespeichert."
    ""
    "  /msg <Name> <Nachricht>"
    "      Private Nachricht senden (wird nicht"
    "      gespeichert, Empfaenger muss online sein)"
    ""
    "  PF7/PF8  Aeltere/neuere Nachrichten"
    "  PF3      Chat verlassen"))

(lspf:define-screen-update chat (divider)
  (setf divider (format nil "~80,,,'-A" "--- /help "))
  (lspf:set-cursor 20 0)
  (unless (lspf:session-property lspf:*session* :chat-entered)
    (setf (lspf:session-property lspf:*session* :chat-entered) t
          (lspf:session-property lspf:*session* :chat-loading) t))
  (lspf:show-key :pf7 "Aeltere")
  (when (chat-scroll-position)
    (lspf:show-key :pf6 "Neueste")
    (lspf:show-key :pf8 "Neuere")))

(defun chat-channel-id ()
  "Return the chat channel ID for the current session, initializing if needed."
  (or (lspf:session-property lspf:*session* :chat-channel-id)
      (let ((id (default-channel-id)))
        (setf (lspf:session-property lspf:*session* :chat-channel-id) id)
        id)))

(defun chat-scroll-position ()
  "Return the current scroll anchor message ID, or NIL for latest."
  (lspf:session-property lspf:*session* :chat-scroll-id))

(defun chat-display-messages ()
  "Return messages to display based on scroll position, plus any private messages."
  (let* ((channel-id (chat-channel-id))
         (scroll-id (chat-scroll-position))
         (db-msgs (if scroll-id
                      (chat-messages-before channel-id (1+ scroll-id)
                                            +chat-display-lines+)
                      (chat-messages-latest channel-id +chat-display-lines+)))
         (private-msgs (consume-private-messages)))
    (if (and (null scroll-id) private-msgs)
        (append db-msgs private-msgs)
        db-msgs)))

(defun current-username ()
  "Return the current session's username."
  (let ((user (session-user lspf:*session*)))
    (when user (user-username user))))

(lspf:define-dynamic-area-updater chat messages ()
  (when (lspf:session-property lspf:*session* :chat-loading)
    (setf (lspf:session-property lspf:*session* :chat-loading) nil)
    (return-from lispf:update-dynamic-area
      (append (make-list (1- +chat-display-lines+) :initial-element "")
              (list (list :content "Daten werden geladen..."
                          :color cl3270:+turquoise+)))))
  (let ((help-p (lspf:session-property lspf:*session* :chat-show-help)))
    (if help-p
        ;; Show help text
        (let ((n (length *chat-help-text*)))
          (append (make-list (- +chat-display-lines+ n) :initial-element "")
                  (mapcar (lambda (line)
                            (list :content line :color cl3270:+turquoise+))
                          *chat-help-text*)))
        ;; Show chat messages
        (let* ((msgs (chat-display-messages))
               (my-name (current-username))
               (lines (format-chat-messages msgs my-name))
               (n (length lines)))
          (if (<= n +chat-display-lines+)
              (append (make-list (- +chat-display-lines+ n) :initial-element "")
                      lines)
              (last lines +chat-display-lines+))))))

(defun parse-chat-input (input1 input2)
  "Combine two input lines into a single message string, trimming trailing blanks."
  (let* ((line1 (string-right-trim '(#\Space) input1))
         (line2 (string-right-trim '(#\Space) input2)))
    (if (plusp (length line2))
        (concatenate 'string line1 (string #\Newline) line2)
        line1)))

(lspf:define-key-handler chat :enter (input1 input2)
  ;; Dismiss help if showing
  (when (lspf:session-property lspf:*session* :chat-show-help)
    (setf (lspf:session-property lspf:*session* :chat-show-help) nil))
  (let ((text (parse-chat-input (or input1 "") (or input2 "")))
        (context (lspf:session-context lspf:*session*)))
    ;; Clear input fields with spaces (needed for no-clear redisplay)
    (let ((blank (make-string 80 :initial-element #\Space)))
      (setf (gethash "input1" context) blank
            (gethash "input2" context) blank
            (gethash "input1" lspf:*current-field-values*) blank
            (gethash "input2" lspf:*current-field-values*) blank))
    (when (string= (string-trim '(#\Space) text) "")
      (return-from lspf:handle-key :stay))
    ;; Handle /help command
    (when (string-equal (string-trim '(#\Space) text) "/help")
      (setf (lspf:session-property lspf:*session* :chat-show-help) t)
      (return-from lspf:handle-key :stay))
    ;; Handle /msg command
    (when (and (>= (length text) 5)
               (string-equal (subseq text 0 4) "/msg"))
      (let* ((rest (string-trim '(#\Space) (subseq text 4)))
             (space-pos (position #\Space rest)))
        (unless space-pos
          (lspf:application-error "/msg <Name> <Nachricht>"))
        (let ((recipient (subseq rest 0 space-pos))
              (msg (string-trim '(#\Space) (subseq rest (1+ space-pos)))))
          (when (string= msg "")
            (lspf:application-error "/msg <Name> <Nachricht>"))
          (let ((user (session-user lspf:*session*)))
            (unless (deliver-private-message user recipient msg)
              (lspf:application-error
               (format nil "~A ist nicht online" recipient))))
          (return-from lspf:handle-key :stay))))
    ;; Regular message
    (let ((user (session-user lspf:*session*)))
      (add-chat-message (chat-channel-id) user text))
    (setf (lspf:session-property lspf:*session* :chat-scroll-id) nil))
  :stay)

(lspf:define-key-handler chat :pf1 ()
  (setf (lspf:session-property lspf:*session* :chat-show-help)
        (not (lspf:session-property lspf:*session* :chat-show-help)))
  :stay)

(lspf:define-key-handler chat :pf6 ()
  ;; Jump to newest messages
  (setf (lspf:session-property lspf:*session* :chat-scroll-id) nil
        (lspf:session-property lspf:*session* :chat-show-help) nil)
  :stay)

(lspf:define-key-handler chat :pf7 ()
  (when (lspf:session-property lspf:*session* :chat-show-help)
    (setf (lspf:session-property lspf:*session* :chat-show-help) nil))
  (let* ((channel-id (chat-channel-id))
         (scroll-id (chat-scroll-position))
         (msgs (if scroll-id
                   (chat-messages-before channel-id (1+ scroll-id)
                                         +chat-display-lines+)
                   (chat-messages-latest channel-id +chat-display-lines+))))
    (when msgs
      (setf (lspf:session-property lspf:*session* :chat-scroll-id)
            (getf (first msgs) :id))))
  :stay)

(lspf:define-key-handler chat :pf8 ()
  (when (lspf:session-property lspf:*session* :chat-show-help)
    (setf (lspf:session-property lspf:*session* :chat-show-help) nil))
  (let ((scroll-id (chat-scroll-position)))
    (unless scroll-id
      (return-from lspf:handle-key :stay))
    (let* ((channel-id (chat-channel-id))
           (newer (chat-messages-after channel-id scroll-id +chat-display-lines+)))
      (if newer
          (let ((newest-id (getf (car (last newer)) :id))
                (channel-newest (chat-newest-id channel-id)))
            (if (>= newest-id channel-newest)
                (setf (lspf:session-property lspf:*session* :chat-scroll-id) nil)
                (setf (lspf:session-property lspf:*session* :chat-scroll-id)
                      newest-id)))
          (setf (lspf:session-property lspf:*session* :chat-scroll-id) nil))))
  :stay)

;;; Login log screen

(lspf:define-list-data-getter protokoll (start end)
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

(defun start (&key (port 3270) (host "127.0.0.1"))
  "Start the VERON application on PORT."
  (initialize-db)
  (lspf:start-application *veron-app* :port port :host host))
