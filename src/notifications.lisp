;;; -*- Mode: Lisp -*-

;;; Notification system: decoupled signaling and delivery.
;;; Supports ntfy.sh push notifications and in-app local delivery.

(in-package #:veron)

;;; Configuration

(defvar *ntfy-base-url* "https://ntfy.sh/"
  "Base URL for the ntfy service.")

;;; Known event types

(defparameter *notification-events*
  '((:guestbook . "Gaestebuch")
    (:login . "Anmeldung")
    (:logout . "Abmeldung"))
  "Alist of (keyword . display-label) for notification event types.")

;;; Event conversion

(defun event-to-string (event)
  "Convert an event keyword to its database string representation."
  (string-downcase (symbol-name event)))

(defun string-to-event (string)
  "Convert a database string to an event keyword."
  (intern (string-upcase string) :keyword))

;;; Notification settings (per-user, per-event)

(defun load-notification-settings (user-id)
  "Load all notification settings for USER-ID.
Returns an alist of (event-keyword . (:ntfy bool :local bool))."
  (with-db
    (let ((rows (pomo:query
                 "SELECT event, ntfy, local FROM notification_settings WHERE user_id = $1"
                 user-id :rows)))
      (loop for (event ntfy local) in rows
            collect (cons (string-to-event event)
                          (list :ntfy ntfy :local local))))))

(defun save-notification-setting (user-id event ntfy local)
  "Save a single notification setting. EVENT is a keyword."
  (with-db
    (pomo:execute
     "INSERT INTO notification_settings (user_id, event, ntfy, local)
      VALUES ($1, $2, $3, $4)
      ON CONFLICT (user_id, event) DO UPDATE SET ntfy = $3, local = $4"
     user-id (event-to-string event) ntfy local)))

(defun load-ntfy-topic (user-id)
  "Return the ntfy topic for USER-ID, or empty string."
  (with-db
    (or (pomo:query "SELECT ntfy_topic FROM users WHERE id = $1"
                    user-id :single)
        "")))

(defun save-ntfy-topic (user-id topic)
  "Save the ntfy topic for USER-ID."
  (with-db
    (pomo:execute "UPDATE users SET ntfy_topic = $2 WHERE id = $1"
                  user-id topic)))

(defun load-notification-beep (user-id)
  "Return T if USER-ID has notification beep enabled."
  (with-db
    (pomo:query "SELECT notification_beep FROM users WHERE id = $1"
                user-id :single)))

(defun save-notification-beep (user-id beep)
  "Save the notification beep preference for USER-ID."
  (with-db
    (pomo:execute "UPDATE users SET notification_beep = $2 WHERE id = $1"
                  user-id beep)))

;;; Notification inbox

(defun add-to-inbox (user-id event title message)
  "Insert a notification into the user's inbox."
  (with-db
    (pomo:query
     "INSERT INTO notification_inbox (user_id, event, title, message)
      VALUES ($1, $2, $3, $4) RETURNING id"
     user-id (event-to-string event) title message :single)))

(defun list-inbox (user-id &key unseen-only (limit 50))
  "List notifications for USER-ID, newest first."
  (with-db
    (if unseen-only
        (pomo:query
         "SELECT id, event, title, message, seen, created_at
          FROM notification_inbox WHERE user_id = $1 AND seen = FALSE
          ORDER BY created_at DESC LIMIT $2"
         user-id limit :plists)
        (pomo:query
         "SELECT id, event, title, message, seen, created_at
          FROM notification_inbox WHERE user_id = $1
          ORDER BY created_at DESC LIMIT $2"
         user-id limit :plists))))

(defun mark-inbox-seen (user-id &optional id)
  "Mark notification(s) as seen. If ID is given, mark just that one; otherwise all."
  (with-db
    (if id
        (pomo:execute
         "UPDATE notification_inbox SET seen = TRUE WHERE id = $1 AND user_id = $2"
         id user-id)
        (pomo:execute
         "UPDATE notification_inbox SET seen = TRUE WHERE user_id = $1"
         user-id))))

(defun clear-inbox (user-id)
  "Delete all notifications for USER-ID."
  (with-db
    (pomo:execute "DELETE FROM notification_inbox WHERE user_id = $1" user-id)))

(defun unseen-count (user-id)
  "Return number of unseen notifications for USER-ID."
  (with-db
    (pomo:query
     "SELECT COUNT(*) FROM notification_inbox WHERE user_id = $1 AND seen = FALSE"
     user-id :single)))

;;; ntfy delivery

(defun topic-url (topic)
  "Construct a full ntfy URL from a topic name."
  (concatenate 'string *ntfy-base-url* topic))

(defun send-ntfy (topic title message)
  "Send a notification to a ntfy topic. Returns T on success, NIL on error."
  (handler-case
      (progn
        (dexador:post (topic-url topic)
                      :content message
                      :headers `(("Title" . ,title))
                      :connect-timeout 10
                      :read-timeout 15)
        t)
    (error (e)
      (lispf:log-message :error "ntfy error for ~A: ~A" topic e)
      nil)))

;;; Delivery thread

(defvar *notification-queue* (sb-concurrency:make-mailbox :name "notification-queue")
  "Thread-safe queue for pending notifications.")

(defvar *delivery-thread* nil
  "The background notification delivery thread.")

(defvar *delivery-running* nil
  "Flag to signal the delivery thread to stop.")

(defun process-notification (item)
  "Process a single queued notification. Called by the delivery thread."
  (let ((event (getf item :event))
        (title (getf item :title))
        (message (getf item :message))
        (originator (getf item :originator)))
    (with-db
      (let ((settings (pomo:query
                       "SELECT ns.user_id, ns.ntfy, ns.local, u.ntfy_topic
                        FROM notification_settings ns
                        JOIN users u ON u.id = ns.user_id
                        WHERE ns.event = $1 AND (ns.ntfy = TRUE OR ns.local = TRUE)"
                       (event-to-string event) :rows)))
        (dolist (row settings)
          (destructuring-bind (user-id ntfy-p local-p ntfy-topic) row
            (unless (and originator (= user-id originator))
              (when (and ntfy-p ntfy-topic (plusp (length ntfy-topic)))
                (handler-case
                    (send-ntfy ntfy-topic title message)
                  (error (e)
                    (lispf:log-message :error "ntfy delivery failed for user ~A: ~A"
                                       user-id e))))
              (when local-p
                (add-to-inbox user-id event title message)
                (deliver-to-session user-id)))))))))

(defun deliver-to-session (user-id)
  "Push a pending notification signal to online sessions of USER-ID."
  (let ((app *veron-app*))
    (bt:with-lock-held ((lispf:application-connections-lock app))
      (dolist (conn (lispf:application-connections app))
        (let ((session (lispf:connection-session conn)))
          (when (and session
                     (typep session 'authenticated-session)
                     (= (user-id (session-user session)) user-id))
            (setf (lispf:session-property session :notification-pending) t)
            (let ((lock (lispf:connection-update-lock conn))
                  (cond (lispf:connection-update-cond conn)))
              (bt:with-lock-held (lock)
                (bt:condition-notify cond)))))))))

(defun delivery-loop ()
  "Main loop for the delivery thread. Drains the queue and processes notifications."
  (loop while *delivery-running* do
    (let ((item (sb-concurrency:receive-message *notification-queue*)))
      (when item
        (handler-case
            (process-notification item)
          (error (e)
            (lispf:log-message :error "notification delivery error: ~A" e)))))))

(defun start-delivery-thread ()
  "Start the background notification delivery thread.
Stops any existing thread first to prevent duplicates on hot reload."
  (when (and *delivery-thread* (bt:thread-alive-p *delivery-thread*))
    (stop-delivery-thread))
  (setf *delivery-running* t
        *delivery-thread*
        (bt:make-thread #'delivery-loop :name "veron-notification-delivery")))

(defun stop-delivery-thread ()
  "Stop the background notification delivery thread."
  (setf *delivery-running* nil)
  (sb-concurrency:send-message *notification-queue* nil)
  (when (and *delivery-thread* (bt:thread-alive-p *delivery-thread*))
    (bt:join-thread *delivery-thread*)
    (setf *delivery-thread* nil)))

(defun ensure-delivery-thread ()
  "Ensure the delivery thread is running. Restart if dead."
  (when (or (null *delivery-thread*)
            (not (bt:thread-alive-p *delivery-thread*)))
    (start-delivery-thread)))

;;; Public API

(defun notify (event title message &key originator-user-id)
  "Queue a notification for background delivery.
EVENT is a keyword (:guestbook, :login, :logout).
ORIGINATOR-USER-ID, if provided, will not receive the notification."
  (ensure-delivery-thread)
  (sb-concurrency:send-message
   *notification-queue*
   (list :event event :title title :message message :originator originator-user-id)))

;;; Indicator update

(defun update-notification-indicator ()
  "Update the MSG indicator for the current session based on unseen inbox count.
Call this from screen updates or after marking notifications as seen."
  (when (typep lispf:*session* 'authenticated-session)
    (let ((count (unseen-count (user-id (session-user lispf:*session*)))))
      (if (plusp count)
          (lispf:set-indicator "NOTI" (format nil "NOTI:~2,'0D" count))
          (lispf:clear-indicator "NOTI")))))

;;; In-app delivery helpers (called from update-cycle-hook in veron.lisp)

(defun deliver-notification-to-error-line ()
  "Check for pending notifications and deliver to the error line if empty.
Called from the update cycle hook in the session's update thread."
  (when (lispf:session-property lispf:*session* :notification-pending)
    (setf (lispf:session-property lispf:*session* :notification-pending) nil)
    (when (typep lispf:*session* 'authenticated-session)
      (let* ((uid (user-id (session-user lispf:*session*)))
             (unseen (list-inbox uid :unseen-only t :limit 1)))
        ;; Update NOTI indicator
        (update-notification-indicator)
        ;; Deliver to error line if no message is currently shown
        (when (and unseen (not (lispf:session-property lispf:*session* :message-line)))
          (let* ((entry (first unseen))
                 (msg (or (getf entry :message) ""))
                 (text (subseq msg 0 (min 79 (length msg))))
                 (beep-p (load-notification-beep uid)))
            (lispf:set-message :notification text)
            (setf (lispf:session-property lispf:*session* :notification-displayed-id)
                  (getf entry :id))
            (lispf:send-error-line-overlay text :alarm beep-p
                                             :color cl3270:+yellow+)))))))
