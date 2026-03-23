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

(defmethod lspf:session-idle-timeout ((app (eql *veron-app*)) session)
  (case (lspf:session-current-screen session)
    (login 60)
    (otherwise nil)))

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
  'logout)

;;; Logout confirmation

(lspf:define-key-handler logout :pf5 ()
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
         (idle (format-duration (- now (lspf:session-last-activity session))))
         (connected (format-duration (- now (session-connect-time session))))
         (tls (if (lspf:session-tls-p session) "Yes" "")))
    (format nil "~3D. ~16A ~8A ~15A ~8A ~10A ~A"
            (1+ index) username app-name screen idle connected tls)))

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

(lspf:define-list-data-getter guestbook (start end)
  (let* ((total (guestbook-count))
         (entries (guestbook-entries start (- end start))))
    (values (loop for e in entries
                  collect (list :author (getf e :author)
                                :date (let ((ts (getf e :created-at)))
                                        (if ts (format-datetime ts) ""))
                                :preview (substitute #\Space #\Newline
                                                     (getf e :message))))
            total)))

(lspf:define-key-handler guestbook :enter ()
  (let ((index (lspf:selected-list-index)))
    (when index
      (let* ((entries (guestbook-entries index 1))
             (entry (first entries)))
        (when entry
          (setf (lspf:session-property lspf:*session* :browse-entry) entry)
          (setf (lspf:session-property lspf:*session* :browse-index) index)
          'guestbook-entry)))))

(lspf:define-screen-update guestbook-entry (author date message)
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
         (entries (when (and new-index (>= new-index 0))
                    (guestbook-entries new-index 1)))
         (entry (first entries)))
    (when entry
      (setf (lspf:session-property lspf:*session* :browse-entry) entry
            (lspf:session-property lspf:*session* :browse-index) new-index
            (lspf:session-property lspf:*session* :force-redraw) t)))
  :stay)

(lspf:define-key-handler guestbook-entry :pf7 ()
  (browse-guestbook-entry -1))

(lspf:define-key-handler guestbook-entry :pf8 ()
  (browse-guestbook-entry 1))

(lspf:define-key-handler guestbook-entry :pf5 ()
  (let ((user (session-user lspf:*session*)))
    (unless (admin-p user)
      (lspf:application-error "Keine Berechtigung"))
    (let ((entry (lspf:session-property lspf:*session* :browse-entry)))
      (when entry
        (setf (lspf:session-property lspf:*session* :confirm-delete)
              (getf entry :id))
        'guestbook-delete))))

;;; Guestbook delete confirmation

(lspf:define-key-handler guestbook-delete :pf5 ()
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
             :back)  ; back to guestbook-entry
            (t
             ;; No more entries, return to list
             (setf (lspf:session-property lspf:*session* :browse-entry) nil)
             (pop (lspf:session-screen-stack lspf:*session*))
             (setf (lspf:list-offset lspf:*session* 'guestbook) 0)
             (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
                   "Eintrag geloescht")
             :back)))))))

;;; Guestbook new entry

(lspf:define-screen-update guestbook-new (author message)
  (let ((user (session-user lspf:*session*)))
    (when (and user (or (null author) (string= author "")))
      (setf author (user-username user)))
    (when user
      (lspf:set-field-attribute "author" :write nil :intense t)))
  ;; Restore message from session property when returning from confirmation
  (let ((saved-message (lspf:session-property lspf:*session* :new-entry-message)))
    (when (and saved-message (string= message ""))
      (setf message saved-message))))

(lspf:define-key-handler guestbook-new :enter ()
  (let ((next-row (min (1+ (lspf:cursor-row)) 20)))
    (lspf:set-cursor next-row 0))
  :stay)

(lspf:define-key-handler guestbook-new :pf5 (author message)
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
    'guestbook-confirm))

;;; Guestbook save confirmation

(lspf:define-screen-update guestbook-confirm (author message)
  (setf author (lspf:session-property lspf:*session* :new-entry-author))
  (setf message (lspf:session-property lspf:*session* :new-entry-message)))

(lspf:define-key-handler guestbook-confirm :pf5 ()
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
      (setf (lspf:list-offset lspf:*session* 'guestbook) 0)
      ;; Set confirmation message for the guestbook list
      (setf (gethash "errormsg" (lspf:session-context lspf:*session*))
            "Eintrag gespeichert")))
  ;; Pop back past guestbook-new to the guestbook list
  (pop (lspf:session-screen-stack lspf:*session*))
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

;;; Chat indicators

(defun count-chat-users ()
  "Count sessions currently on the chat screen."
  (let ((count 0))
    (bt:with-lock-held ((lspf::application-sessions-lock lspf:*application*))
      (dolist (s (lspf::application-sessions lspf:*application*))
        (when (and (eq (lspf:session-current-screen s) 'chat)
                   (not (lspf:session-property s :chat-leaving)))
          (incf count))))
    count))

(defun format-chat-indicator (user-count pm-pending)
  "Format combined chat indicator: CHAT:xxi where xx=users, i=PM flag."
  (format nil "CHAT:~2,'0D~A" (min user-count 99) (if pm-pending "*" " ")))

(defun update-chat-indicators ()
  "Update chat indicator on all sessions with current count and per-session PM flag."
  (let ((count (count-chat-users)))
    (lspf:broadcast
     (lambda ()
       (lspf:set-indicator "chat"
         (format-chat-indicator count
           (lspf:session-property lspf:*session* :chat-pm-pending)))))))

(defun update-my-chat-indicator ()
  "Update just the current session's chat indicator."
  (lspf:set-indicator "chat"
    (format-chat-indicator (count-chat-users)
      (lspf:session-property lspf:*session* :chat-pm-pending))))

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
    "  /who     Wer ist im Chat?"
    "  /quit    Chat verlassen"
    ""
    "  PF7/PF8  Aeltere/neuere Nachrichten"
    "  PF3      Chat verlassen"))

(lspf:define-screen-update chat (divider)
  (setf divider (format nil "~80,,,'-A" "--- /help "))
  (lspf:set-cursor 20 0)
  (setf (lspf:session-property lspf:*session* :chat-leaving) nil)
  (let ((entering (not (lspf:session-property lspf:*session* :chat-entered))))
    (unless (lspf:session-property lspf:*session* :chat-entered)
      (setf (lspf:session-property lspf:*session* :chat-entered) t
            (lspf:session-property lspf:*session* :chat-loading) t))
    (update-chat-indicators)
    (when entering
      (let ((name (current-username)))
        (when name
          (broadcast-chat-notification
           (format nil "--- ~A hat den Chat betreten" name)
           lspf:*session*)))))
  (let ((total (length (chat-all-formatted-lines))))
    (when (> total +chat-display-lines+)
      (lspf:show-key :pf7 "Aeltere"))
    (when (chat-scroll-offset)
      (lspf:show-key :pf6 "Neueste")
      (lspf:show-key :pf8 "Neuere"))))

(defun chat-channel-id ()
  "Return the chat channel ID for the current session, initializing if needed."
  (or (lspf:session-property lspf:*session* :chat-channel-id)
      (let ((id (default-channel-id)))
        (setf (lspf:session-property lspf:*session* :chat-channel-id) id)
        id)))

(defun chat-scroll-offset ()
  "Return the scroll offset (end line index), or NIL for latest."
  (lspf:session-property lspf:*session* :chat-scroll-offset))

(defun chat-all-formatted-lines ()
  "Format all messages in the per-user buffer into display lines.
Returns the full list of formatted lines."
  (let* ((channel-id (chat-channel-id))
         (msgs (coerce (sync-user-chat-buffer channel-id) 'list)))
    (format-chat-messages msgs :start-of-log t)))

(defun current-username ()
  "Return the current session's username."
  (let ((user (session-user lspf:*session*)))
    (when user (user-username user))))

(lspf:define-dynamic-area-updater chat messages ()
  (let* ((all-lines (chat-all-formatted-lines))
         (total (length all-lines))
         (offset (chat-scroll-offset)))
    (when (and (not offset)
               (lspf:session-property lspf:*session* :chat-pm-pending))
      (setf (lspf:session-property lspf:*session* :chat-pm-pending) nil)
      (update-my-chat-indicator))
    (cond
      ;; Scrolled back: show page ending at offset
      (offset
       (let* ((end (min offset total))
              (start (max 0 (- end +chat-display-lines+)))
              (page (subseq all-lines start end))
              (n (length page)))
         (if (< n +chat-display-lines+)
             (append page
                     (make-list (- +chat-display-lines+ n) :initial-element ""))
             page)))
      ;; Live view: bottom-align last page
      ((<= total +chat-display-lines+)
       (append (make-list (- +chat-display-lines+ total) :initial-element "")
               all-lines))
      (t (last all-lines +chat-display-lines+)))))

(defun parse-chat-input (input1 input2)
  "Combine two input lines into a single message string.
Treats the second line as a continuation of the first (no newline inserted)."
  (let* ((line1 (string-right-trim '(#\Space) input1))
         (line2 (string-right-trim '(#\Space) input2)))
    (if (plusp (length line2))
        (concatenate 'string line1 line2)
        line1)))

(defun clear-chat-input ()
  "Clear the chat input fields for no-clear redisplay."
  (let ((blank (make-string 80 :initial-element #\Space))
        (context (lspf:session-context lspf:*session*)))
    (setf (gethash "input1" context) blank
          (gethash "input2" context) blank)))

(lspf:define-key-handler chat :enter (input1 input2)
  (let ((text (parse-chat-input (or input1 "") (or input2 ""))))
    (clear-chat-input)
    (when (string= (string-trim '(#\Space) text) "")
      (return-from lspf:handle-key :stay))
    ;; Handle /help command
    (when (string-equal (string-trim '(#\Space) text) "/help")
      (let ((buf (user-chat-buffer))
            (now (get-universal-time)))
        (dolist (line *chat-help-text*)
          (vector-push-extend (list :message line :created-at now :notification t)
                              buf)))
      (return-from lspf:handle-key :stay))
    ;; Handle /who command
    (when (string-equal (string-trim '(#\Space) text) "/who")
      (let ((names '()))
        (bt:with-lock-held ((lspf::application-sessions-lock lspf:*application*))
          (dolist (s (lspf::application-sessions lspf:*application*))
            (when (eq (lspf:session-current-screen s) 'chat)
              (let ((user (session-user s)))
                (when user (push (user-username user) names))))))
        (let ((msg (list :message (format nil "--- Im Chat: ~{~A~^, ~}"
                                          (sort names #'string-lessp))
                         :created-at (get-universal-time)
                         :notification t)))
          (vector-push-extend msg (user-chat-buffer))))
      (return-from lspf:handle-key :stay))
    ;; Handle /quit command
    (when (string-equal (string-trim '(#\Space) text) "/quit")
      (let ((name (current-username)))
        (setf (lspf:session-property lspf:*session* :chat-leaving) t
              (lspf:session-property lspf:*session* :chat-entered) nil)
        (update-chat-indicators)
        (when name
          (broadcast-chat-notification
           (format nil "--- ~A hat den Chat verlassen" name)
           lspf:*session*)))
      (return-from lspf:handle-key :back))
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
               (format nil "~A ist nicht online" recipient)))
            (send-own-private-message user recipient msg text))
          (return-from lspf:handle-key :stay))))
    ;; Unknown / command
    (when (and (plusp (length text)) (char= (char text 0) #\/))
      (let* ((cmd (string-trim '(#\Space) text))
             (end (or (position #\Space cmd) (length cmd)))
             (msg (list :message (format nil "--- Unbekannter Befehl: ~A" (subseq cmd 0 end))
                        :created-at (get-universal-time)
                        :notification t)))
        (vector-push-extend msg (user-chat-buffer)))
      (return-from lspf:handle-key :stay))
    ;; Regular message — add locally and to shared buffer
    (let ((user (session-user lspf:*session*)))
      (add-own-message (chat-channel-id) user text text))
    (setf (lspf:session-property lspf:*session* :chat-scroll-offset) nil))
  :stay)

(lspf:define-key-handler chat :pf3 ()
  (let ((name (current-username)))
    (setf (lspf:session-property lspf:*session* :chat-leaving) t
          (lspf:session-property lspf:*session* :chat-entered) nil)
    (update-chat-indicators)
    (when name
      (broadcast-chat-notification
       (format nil "--- ~A hat den Chat verlassen" name)
       lspf:*session*)))
  :back)

(lspf:define-key-handler chat :pf6 ()
  (setf (lspf:session-property lspf:*session* :chat-scroll-offset) nil)
  :stay)

(lspf:define-key-handler chat :pf7 ()
  (let* ((all-lines (chat-all-formatted-lines))
         (total (length all-lines))
         (offset (or (chat-scroll-offset) total))
         (new-offset (max +chat-display-lines+ (- offset +chat-display-lines+))))
    (when (< new-offset offset)
      (setf (lspf:session-property lspf:*session* :chat-scroll-offset)
            new-offset)))
  :stay)

(lspf:define-key-handler chat :pf8 ()
  (let ((offset (chat-scroll-offset)))
    (unless offset
      (return-from lspf:handle-key :stay))
    (let* ((all-lines (chat-all-formatted-lines))
           (total (length all-lines))
           (new-offset (+ offset +chat-display-lines+)))
      (if (>= new-offset total)
          (setf (lspf:session-property lspf:*session* :chat-scroll-offset) nil)
          (setf (lspf:session-property lspf:*session* :chat-scroll-offset)
                new-offset))))
  :stay)

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
  "Start a Swank server if SWANK_PORT is set in the environment."
  (alexandria:when-let (port-string (env "SWANK_PORT" nil))
    (let ((port (parse-integer port-string)))
      (swank:create-server :port port :dont-close t)
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
