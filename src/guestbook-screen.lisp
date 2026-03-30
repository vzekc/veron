;;; -*- Mode: Lisp -*-

;;; Guestbook screen handlers.

(in-package #:veron)

;;; Guestbook list

(lispf:define-list-data-getter guestbook (start end)
  (let* ((total (guestbook-count))
         (entries (guestbook-entries start (- end start))))
    (values (loop for e in entries
                  collect (list :author (getf e :author)
                                :date (let ((ts (getf e :created-at)))
                                        (if ts (format-datetime ts) ""))
                                :preview (substitute #\Space #\Newline
                                                     (getf e :message))))
            total)))

(lispf:define-key-handler guestbook :enter ()
  (let ((index (lispf:selected-list-index)))
    (when index
      (let* ((entries (guestbook-entries index 1))
             (entry (first entries)))
        (when entry
          (setf (lispf:session-property lispf:*session* :browse-entry) entry)
          (setf (lispf:session-property lispf:*session* :browse-index) index)
          'guestbook-entry)))))

;;; Guestbook entry view

(lispf:define-screen-update guestbook-entry (author date message)
  (let ((entry (lispf:session-property lispf:*session* :browse-entry)))
    (when entry
      (setf author (getf entry :author)
            date (let ((ts (getf entry :created-at)))
                   (if ts (format-datetime ts) ""))
            message (getf entry :message))))
  (let ((user (session-user lispf:*session*)))
    (when (admin-p user)
      (lispf:show-key :pf5 "Loeschen")))
  (let ((index (lispf:session-property lispf:*session* :browse-index))
        (total (guestbook-count)))
    (when (and index (> index 0))
      (lispf:show-key :pf7 "Vor."))
    (when (and index (< index (1- total)))
      (lispf:show-key :pf8 "Naech."))))

(defun browse-guestbook-entry (direction)
  "Navigate to prev (-1) or next (+1) guestbook entry. Returns :stay."
  (let* ((index (lispf:session-property lispf:*session* :browse-index))
         (new-index (when index (+ index direction)))
         (entries (when (and new-index (>= new-index 0))
                    (guestbook-entries new-index 1)))
         (entry (first entries)))
    (when entry
      (setf (lispf:session-property lispf:*session* :browse-entry) entry
            (lispf:session-property lispf:*session* :browse-index) new-index
            (lispf:session-property lispf:*session* :force-redraw) t)))
  :stay)

(lispf:define-key-handler guestbook-entry :pf7 ()
  (browse-guestbook-entry -1))

(lispf:define-key-handler guestbook-entry :pf8 ()
  (browse-guestbook-entry 1))

(lispf:define-key-handler guestbook-entry :pf5 ()
  (let ((user (session-user lispf:*session*)))
    (unless (admin-p user)
      (lispf:application-error "Keine Berechtigung"))
    (let ((entry (lispf:session-property lispf:*session* :browse-entry)))
      (when entry
        (let ((entry-id (getf entry :id))
              (index (lispf:session-property lispf:*session* :browse-index)))
          (lispf:request-confirmation
           "Gaestebucheintrag loeschen?"
           (lambda ()
             (delete-guestbook-entry entry-id)
             ;; Try to show the next entry (which now occupies the same index)
             (let* ((entries (guestbook-entries index 1))
                    (next (first entries)))
               (cond
                 (next
                  (setf (lispf:session-property lispf:*session* :browse-entry) next)
                  (lispf:set-message :confirmation "Eintrag geloescht")
                  :stay)
                 (t
                  (setf (lispf:session-property lispf:*session* :browse-entry) nil
                        (lispf:list-offset lispf:*session* 'guestbook) 0)
                  (lispf:set-message :confirmation "Eintrag geloescht")
                  :back))))))))))

;;; Guestbook new entry

(lispf:define-screen-update guestbook-new (author message)
  (let ((user (session-user lispf:*session*)))
    (if user
        (progn
          (setf author (user-username user))
          (lispf:set-field-attribute "author" :write nil :intense t))
        (setf author "")))
  ;; Restore message from session property when returning from confirmation
  (let ((saved-message (lispf:session-property lispf:*session* :new-entry-message)))
    (when (and saved-message (string= message ""))
      (setf message saved-message))))

(lispf:define-key-handler guestbook-new :enter ()
  (let ((next-row (min (1+ (lispf:cursor-row)) 20)))
    (lispf:set-cursor next-row 0))
  :stay)

(lispf:define-key-handler guestbook-new :pf5 (author message)
  (let ((user (session-user lispf:*session*)))
    (when (string= message "")
      (lispf:application-error "Bitte Nachricht eingeben"))
    (let ((name (if user
                    (user-username user)
                    (if (string= author "")
                        (lispf:application-error "Bitte Name eingeben")
                        (format nil "~A (Gast)" author)))))
      (setf (lispf:session-property lispf:*session* :new-entry-author) name)
      (setf (lispf:session-property lispf:*session* :new-entry-message) message))
    'guestbook-confirm))

;;; Guestbook save confirmation

(lispf:define-screen-update guestbook-confirm (author message)
  (setf author (lispf:session-property lispf:*session* :new-entry-author))
  (setf message (lispf:session-property lispf:*session* :new-entry-message)))

(lispf:define-key-handler guestbook-confirm :pf5 ()
  (let ((author (lispf:session-property lispf:*session* :new-entry-author))
        (message (lispf:session-property lispf:*session* :new-entry-message)))
    (when (and author message)
      (add-guestbook-entry author message)
      (notify :guestbook "Neuer Gaestebucheintrag"
              (format nil "Neuer Gaestebucheintrag von ~A" author)
              :originator-user-id (when (typep lispf:*session* 'authenticated-session)
                                    (user-id (session-user lispf:*session*))))
      (setf (lispf:session-property lispf:*session* :new-entry-author) nil)
      (setf (lispf:session-property lispf:*session* :new-entry-message) nil)
      ;; Reset list offset so the new entry is visible at the top
      (setf (lispf:list-offset lispf:*session* 'guestbook) 0)
      ;; Set confirmation message for the guestbook list
      (lispf:set-message :confirmation "Eintrag gespeichert")))
  ;; Pop back past guestbook-new to the guestbook list
  (pop (lispf:session-screen-stack lispf:*session*))
  :back)
