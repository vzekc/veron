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

(defclass veron-session (lspf:session)
  ((user :initform nil :accessor session-user)
   (term-type :initform "" :accessor session-term-type)
   (connect-time :initform (get-universal-time) :reader session-connect-time)
   (login-id :initform nil :accessor session-login-id)))

;;; Application definition

(lspf:define-application *veron-app*
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
            (record-login user :terminal-type (session-term-type lspf:*session*))))
    'main))

(lspf:define-key-handler login :pf3 ()
  :logoff)

;;; Main screen

(lspf:define-key-handler main :pf3 ()
  'abmelden)

;;; Logout confirmation

(lspf:define-key-handler abmelden :pf5 ()
  (record-logout (session-login-id lspf:*session*))
  :logoff)

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
