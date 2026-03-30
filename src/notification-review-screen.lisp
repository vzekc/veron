;;; -*- Mode: Lisp -*-

;;; Notification inbox review screen.

(in-package #:veron)

(defun format-inbox-timestamp (ts)
  "Format a timestamp for the inbox list."
  (format-datetime ts))

(defun inbox-page-offset ()
  "Return the current page offset for the inbox screen."
  (or (lispf:session-property lispf:*session* :inbox-offset) 0))

(defun set-inbox-page-offset (offset)
  (setf (lispf:session-property lispf:*session* :inbox-offset) offset))

(lispf:define-screen-update noti ()
  (update-notification-indicator))

(lispf:define-dynamic-area-updater noti inbox ()
  (when (typep lispf:*session* 'authenticated-session)
    (let* ((uid (user-id (session-user lispf:*session*)))
           (entries (list-inbox uid :limit 18))
           (offset (inbox-page-offset))
           (page (subseq entries (min offset (length entries))
                         (min (+ offset 18) (length entries)))))
      (lispf:session-property lispf:*session* :inbox-entries entries)
      (loop for i from 0 below 18
            collect (if (< i (length page))
                        (let* ((entry (nth i page))
                               (seen (getf entry :seen))
                               (ts (format-inbox-timestamp (getf entry :created-at)))
                               (msg (or (getf entry :message) ""))
                               (line (format nil "~17A  ~A" ts
                                             (subseq msg 0 (min 58 (length msg))))))
                          (if seen
                              line
                              (list :content line :intense t)))
                        "")))))

(lispf:define-key-handler noti :enter ()
  (when (typep lispf:*session* 'authenticated-session)
    (let* ((entries (lispf:session-property lispf:*session* :inbox-entries))
           (offset (inbox-page-offset))
           (row (- (lispf:cursor-row) 2))
           (index (+ offset row)))
      (when (and (>= row 0) (< index (length entries)))
        (let ((entry (nth index entries)))
          (mark-inbox-seen (user-id (session-user lispf:*session*))
                           (getf entry :id))
          (update-notification-indicator)))))
  :stay)

(lispf:define-key-handler noti :pf5 ()
  (when (typep lispf:*session* 'authenticated-session)
    (mark-inbox-seen (user-id (session-user lispf:*session*)))
    (update-notification-indicator)
    (lispf:set-message :confirmation "Alle als gelesen markiert"))
  :stay)

(lispf:define-key-handler noti :pf9 ()
  (when (typep lispf:*session* 'authenticated-session)
    (lispf:request-confirmation
     "Alle Nachrichten loeschen?"
     (lambda ()
       (clear-inbox (user-id (session-user lispf:*session*)))
       (update-notification-indicator)
       (lispf:set-message :confirmation "Alle Nachrichten geloescht")
       :stay))))
