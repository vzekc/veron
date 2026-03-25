;;; -*- Mode: Lisp -*-

;;; Guestbook screen handlers.

(in-package #:veron)

;;; Guestbook list

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

;;; Guestbook entry view

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
    (when user
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
