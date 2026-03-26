;;; -*- Mode: Lisp -*-

;;; Notification system via ntfy (https://ntfy.sh)

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

(defun events-to-pg-array (events)
  "Convert a list of event keywords to a PostgreSQL text array literal."
  (format nil "{~{~A~^,~}}" (mapcar #'event-to-string events)))

(defun pg-array-to-events (pg-array)
  "Convert a PostgreSQL text array to a list of event keywords."
  (when (and pg-array (not (db-null-p pg-array)))
    (let ((strings (etypecase pg-array
                     (string
                      (let ((trimmed (string-trim '(#\{ #\}) pg-array)))
                        (when (plusp (length trimmed))
                          (uiop:split-string trimmed :separator ","))))
                     (vector (coerce pg-array 'list))
                     (list pg-array))))
      (mapcar #'string-to-event strings))))

;;; Subscription management

(defun subscribe (user-id topic events)
  "Create a notification subscription. EVENTS is a list of event keywords."
  (with-db
    (pomo:query
     "INSERT INTO notification_subscriptions (user_id, topic, events)
      VALUES ($1, $2, $3::text[]) RETURNING id"
     user-id topic (events-to-pg-array events) :single)))

(defun unsubscribe (subscription-id user-id)
  "Delete a subscription, ensuring it belongs to the user."
  (with-db
    (pomo:execute
     "DELETE FROM notification_subscriptions WHERE id = $1 AND user_id = $2"
     subscription-id user-id)))

(defun user-subscriptions (user-id)
  "List all subscriptions for a user. Events are returned as keyword lists."
  (with-db
    (let ((subs (pomo:query
                 "SELECT id, topic, events, created_at
                  FROM notification_subscriptions WHERE user_id = $1
                  ORDER BY created_at"
                 user-id :plists)))
      (dolist (sub subs subs)
        (setf (getf sub :events)
              (pg-array-to-events (getf sub :events)))))))

;;; Event matching

(defun subscription-matches-p (subscription event)
  "Return T if SUBSCRIPTION covers EVENT (a keyword)."
  (member event (getf subscription :events)))

;;; Delivery

(defun topic-url (topic)
  "Construct a full ntfy URL from a topic name."
  (concatenate 'string *ntfy-base-url* topic))

(defun send-ntfy (topic title message)
  "Send a notification to a ntfy topic. Returns T on success, NIL on error."
  (handler-case
      (progn
        (dexador:post (topic-url topic)
                      :content message
                      :headers `(("Title" . ,title)))
        t)
    (error (e)
      (format *error-output* "~&;;; ntfy error for ~A: ~A~%" topic e)
      nil)))

(defun notify (event title message)
  "Send notifications for EVENT (a keyword) to all matching subscriptions.
Delivery happens in a background thread."
  (let ((subscriptions (with-db
                         (let ((subs (pomo:query
                                      "SELECT id, topic, events
                                       FROM notification_subscriptions"
                                      :plists)))
                           (dolist (sub subs subs)
                             (setf (getf sub :events)
                                   (pg-array-to-events (getf sub :events))))))))
    (let ((targets (remove-if-not
                    (lambda (sub) (subscription-matches-p sub event))
                    subscriptions)))
      (when targets
        (bt:make-thread
         (lambda ()
           (dolist (sub targets)
             (send-ntfy (getf sub :topic) title message)))
         :name "veron-notify")))))
