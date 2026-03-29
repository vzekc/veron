;;; -*- Mode: Lisp -*-

;;; Chat system - multi-channel persistent chat with private messaging
;;;
;;; Messages are kept in RAM for fast access. The database is used for
;;; persistence and recovery only. On startup, messages are loaded from
;;; the DB into the in-memory buffer.

(in-package #:veron)

;;; Channel class

(defclass chat-channel ()
  ((id :initarg :id :reader channel-id)
   (messages :initform (make-array 0 :adjustable t :fill-pointer 0)
             :reader channel-messages-buffer)
   (members :initform nil :accessor channel-members)
   (lock :initform (bt:make-lock "chat-channel") :reader channel-lock)
   (id-counter :initform 0 :accessor channel-id-counter)))

;;; Channel registry

(defvar *chat-channels* (make-hash-table)
  "Channel ID -> chat-channel instance.")

(defvar *chat-channels-lock* (bt:make-lock "chat-channels")
  "Lock protecting *chat-channels*.")

(defun find-channel (id)
  "Find a channel by ID, or NIL if not found."
  (bt:with-lock-held (*chat-channels-lock*)
    (gethash id *chat-channels*)))

(defun find-or-create-channel (id)
  "Find a channel by ID, creating it if it doesn't exist."
  (bt:with-lock-held (*chat-channels-lock*)
    (or (gethash id *chat-channels*)
        (setf (gethash id *chat-channels*)
              (make-instance 'chat-channel :id id)))))

;;; Membership

(defun join-channel (channel session username)
  "Add SESSION to CHANNEL's member list. Idempotent."
  (bt:with-lock-held ((channel-lock channel))
    (unless (assoc session (channel-members channel))
      (push (cons session username) (channel-members channel)))))

(defun leave-channel (channel session)
  "Remove SESSION from CHANNEL's member list. Idempotent."
  (bt:with-lock-held ((channel-lock channel))
    (setf (channel-members channel)
          (remove session (channel-members channel) :key #'car))))

(defun channel-user-names (channel)
  "Return a sorted list of usernames in CHANNEL."
  (let ((names (bt:with-lock-held ((channel-lock channel))
                 (mapcar #'cdr (channel-members channel)))))
    (sort names #'string-lessp)))

(defun channel-member-count (channel)
  "Return the number of members in CHANNEL."
  (bt:with-lock-held ((channel-lock channel))
    (length (channel-members channel))))

(defun make-chat-message (type format &rest args)
  "Create a chat message plist with TYPE (:message or :notification) and formatted text."
  (list :type type
        :message (apply #'format nil format args)
        :created-at (get-universal-time)))

(defun message-type (msg)
  "Return the type keyword of MSG (:message or :notification)."
  (getf msg :type :message))

(defun notification-p (msg)
  "Return T if MSG is a system notification (join/leave)."
  (eq (message-type msg) :notification))

(defun load-chat-from-db ()
  "Load all chat messages from the database into channel instances."
  (dolist (channel-id (chat-channel-ids))
    (let ((channel (find-or-create-channel channel-id))
          (msgs (chat-channel-messages channel-id)))
      (setf msgs
            (mapcar (lambda (msg)
                      (let ((mtype (getf msg :message-type)))
                        (remf msg :message-type)
                        (list* :type (intern (string-upcase (or mtype "message"))
                                             :keyword)
                               msg)))
                    msgs))
      (setf (slot-value channel 'messages)
            (make-array (length msgs)
                        :adjustable t :fill-pointer (length msgs)
                        :initial-contents msgs))
      (when msgs
        (setf (channel-id-counter channel)
              (max (channel-id-counter channel)
                   (getf (car (last msgs)) :id)))))))

(defun channel-messages (channel-id)
  "Return the message vector for CHANNEL-ID."
  (let ((channel (find-or-create-channel channel-id)))
    (channel-messages-buffer channel)))

;;; Channel management

(defun default-channel-id ()
  "Return the ID of the default chat channel."
  (default-chat-channel-id))

;;; Message operations

(defun add-chat-entry (channel-id user type message)
  "Add a message to the shared buffer and persist to DB.
TYPE is :message or :notification. Returns the message plist."
  (let* ((channel (find-or-create-channel channel-id))
         (type-string (string-downcase (symbol-name type)))
         (db-id (insert-chat-message channel-id (user-id user) (user-username user)
                                     message type-string))
         (msg (list* :id db-id
                     :type type
                     :message message
                     :created-at (get-universal-time)
                     (when (eq type :message)
                       (list :username (user-username user))))))
    (bt:with-lock-held ((channel-lock channel))
      (vector-push-extend msg (channel-messages-buffer channel))
      (setf (channel-id-counter channel)
            (max (channel-id-counter channel) db-id)))
    msg))

(defun add-chat-message (channel-id user format &rest args)
  "Add a regular chat message."
  (add-chat-entry channel-id user :message (apply #'format nil format args)))

(defun add-chat-notification (channel-id user format &rest args)
  "Add a system notification message (join/leave)."
  (add-chat-entry channel-id user :notification (apply #'format nil format args)))

;;; Message formatting

(defconstant +chat-display-lines+ 18
  "Number of lines available for chat message display (rows 0-17).")

(defparameter *german-numbers*
  #("null" "eins" "zwei" "drei" "vier" "fuenf" "sechs" "sieben"
    "acht" "neun" "zehn" "elf" "zwoelf" "dreizehn" "vierzehn"
    "fuenfzehn" "sechzehn" "siebzehn" "achtzehn" "neunzehn" "zwanzig"))

(defun number-to-german (n)
  "Convert a non-negative integer to German words."
  (if (<= n 20)
      (aref *german-numbers* n)
      (format nil "~D" n)))

(defun german-unit (n singular plural &key (einer "einer"))
  "Format N with SINGULAR or PLURAL German unit name.
EINER specifies the word for 1 (einer for feminine, einem for dative masculine)."
  (if (= n 1)
      (format nil "~A ~A" einer singular)
      (format nil "~A ~A" (number-to-german n) plural)))

(defun format-silence-duration (gap-seconds)
  "Format a gap duration in German words."
  (let* ((total-minutes (floor gap-seconds 60))
         (days (floor total-minutes (* 24 60)))
         (hours (floor (mod total-minutes (* 24 60)) 60))
         (minutes (mod total-minutes 60))
         (parts '()))
    (when (plusp minutes)
      (push (german-unit minutes "Minute" "Minuten") parts))
    (when (plusp hours)
      (push (german-unit hours "Stunde" "Stunden") parts))
    (when (plusp days)
      (push (german-unit days "Tag" "Tagen" :einer "einem") parts))
    (format nil "nach ~{~A~^, ~} Stille" parts)))

(defun format-silence-divider (timestamp gap-seconds)
  "Format a silence divider with absolute time and duration."
  (multiple-value-bind (sec min hour day month year)
      (decode-display-time timestamp)
    (declare (ignore sec))
    (let ((dow (aref #("Montag" "Dienstag" "Mittwoch" "Donnerstag"
                        "Freitag" "Samstag" "Sonntag")
                      (nth-value 6 (decode-display-time timestamp)))))
      (format nil "-- ~A, ~2,'0D.~2,'0D.~4D ~2,'0D:~2,'0D, ~A"
              dow day month year hour min
              (format-silence-duration gap-seconds)))))

(defun wrap-paragraph (text width)
  "Wrap a single paragraph to WIDTH columns, returning a list of lines."
  (let ((lines nil)
        (len (length text))
        (pos 0))
    (loop while (< pos len)
          do (let ((end (min (+ pos width) len)))
               (if (<= end pos)
                   (return)
                   (if (>= end len)
                       (progn (push (subseq text pos) lines)
                              (setf pos len))
                       (let ((break (position #\Space text :end end :from-end t :start pos)))
                         (if (and break (> break pos))
                             (progn (push (subseq text pos break) lines)
                                    (setf pos (1+ break)))
                             (progn (push (subseq text pos end) lines)
                                    (setf pos end))))))))
    (nreverse lines)))

(defun word-wrap (text width)
  "Wrap TEXT to WIDTH columns, respecting existing newlines.
Returns a list of lines."
  (when (zerop (length text))
    (return-from word-wrap (list "")))
  (loop for paragraph in (uiop:split-string text :separator '(#\Newline))
        nconc (if (string= paragraph "")
                  (list "")
                  (wrap-paragraph paragraph width))))

(defun wrap-message-lines (username message &key private)
  "Format a chat message into display lines with word wrapping.
Public: (<nick>) message  Private: *nick* message"
  (let* ((prefix (if private
                     (format nil "*~A* " username)
                     (format nil "(~A) " username)))
         (prefix-len (length prefix))
         (cont-indent (make-string (min prefix-len 20) :initial-element #\Space))
         (first-width (- 80 prefix-len))
         (rest-width (- 80 (length cont-indent)))
         (wrapped (word-wrap message first-width))
         (lines '()))
    (push (concatenate 'string prefix (or (first wrapped) "")) lines)
    (dolist (line (rest wrapped))
      (if (<= (length line) rest-width)
          (push (concatenate 'string cont-indent line) lines)
          (dolist (sub (word-wrap line rest-width))
            (push (concatenate 'string cont-indent sub) lines))))
    (nreverse lines)))

(defconstant +silence-threshold+ (* 5 60)
  "Seconds of silence before inserting a timestamp divider.")

(defun format-absolute-timestamp (universal-time)
  "Format a universal time as a German absolute timestamp header."
  (multiple-value-bind (sec min hour day month year)
      (decode-display-time universal-time)
    (declare (ignore sec))
    (format nil "--- ~A, ~2,'0D.~2,'0D.~4D ~2,'0D:~2,'0D ~44,,,'-A"
            (aref #("Montag" "Dienstag" "Mittwoch" "Donnerstag"
                    "Freitag" "Samstag" "Sonntag")
                  (nth-value 6 (decode-display-time universal-time)))
            day month year hour min "")))

(defun format-chat-messages (messages &key start-of-log preceding-timestamp)
  "Format a list of message plists into display lines.
Inserts a timestamp divider after 5+ minutes of silence.
PRECEDING-TIMESTAMP is the time of the last message before this page,
used to detect gaps at page boundaries.
When START-OF-LOG is true, prepends the absolute timestamp of the first message.
Message colors:
  - Timestamps/dividers: turquoise
  - Outbound (own) messages: green
  - Inbound private messages: yellow
  - Inbound public messages: default
Returns a list of strings or plists (for colored lines)."
  (let ((lines '())
        (last-time preceding-timestamp))
    (when (and start-of-log messages)
      (let ((first-ts (getf (first messages) :created-at)))
        (when first-ts
          (push (list :content (format-absolute-timestamp first-ts)
                      :color cl3270:+turquoise+)
                lines))))
    (dolist (msg messages)
      (let* ((timestamp (getf msg :created-at))
             (username (getf msg :username))
             (private-p (getf msg :private))
             (own-p (getf msg :own)))
        ;; Insert silence divider if gap > 5 minutes
        (when (and last-time timestamp
                   (> (- timestamp last-time) +silence-threshold+))
          (push (list :content (format-silence-divider timestamp (- timestamp last-time))
                      :color cl3270:+turquoise+)
                lines))
        (when timestamp (setf last-time timestamp))
        (let* ((notification-p (notification-p msg))
               (msg-lines (cond (notification-p
                                 (word-wrap (getf msg :message) 80))
                                ((and own-p (getf msg :raw-input))
                                 (word-wrap (getf msg :raw-input) 80))
                                (t (wrap-message-lines username (getf msg :message)
                                                       :private private-p)))))
          (dolist (line msg-lines)
            (push (cond (notification-p (list :content line :color cl3270:+turquoise+))
                        (own-p (list :content line :color cl3270:+green+))
                        (private-p (list :content line :color cl3270:+yellow+))
                        (t (list :content line :color cl3270:+white+)))
                  lines)))))
    (nreverse lines)))


;;; Per-user message buffer

(defun user-chat-buffer ()
  "Return the per-user chat buffer for the current session, creating if needed."
  (or (lispf:session-property lispf:*session* :chat-user-buffer)
      (setf (lispf:session-property lispf:*session* :chat-user-buffer)
            (make-array 0 :adjustable t :fill-pointer 0))))

(defun sync-user-chat-buffer (channel-id)
  "Sync the per-user buffer with new public messages from the shared channel buffer.
Skips messages marked as :own (already in the user buffer from local send).
Returns the per-user buffer."
  (let* ((buf (user-chat-buffer))
         (synced (or (lispf:session-property lispf:*session* :chat-sync-index) 0))
         (shared (channel-messages channel-id))
         (shared-len (length shared))
         (my-name (let ((user (session-user lispf:*session*)))
                    (when user (user-username user)))))
    (when (< synced shared-len)
      (loop for i from synced below shared-len
            for msg = (aref shared i)
            unless (and my-name (string-equal (getf msg :username) my-name)
                        (lispf:session-property lispf:*session* :chat-sent-ids)
                        (member (getf msg :id)
                                (lispf:session-property lispf:*session* :chat-sent-ids)))
            do (vector-push-extend msg buf))
      (setf (lispf:session-property lispf:*session* :chat-sync-index) shared-len))
    buf))

(defun add-own-message (channel-id user message raw-input)
  "Send a message: add to shared buffer, add locally as :own, skip during sync."
  (let ((msg (add-chat-message channel-id user "~A" message)))
    ;; Add to own buffer with :own flag and raw input text
    (let ((own-msg (list* :own t :raw-input raw-input msg)))
      (vector-push-extend own-msg (user-chat-buffer)))
    ;; Track sent IDs so sync skips these
    (push (getf msg :id)
          (lispf:session-property lispf:*session* :chat-sent-ids))))

(defun user-message-count (channel-id)
  "Return the number of messages in the per-user buffer."
  (length (sync-user-chat-buffer channel-id)))

(defun user-messages-slice (channel-id start end)
  "Return messages from the per-user buffer, index START to END (exclusive)."
  (let* ((buf (sync-user-chat-buffer channel-id))
         (len (length buf)))
    (coerce (subseq buf (max 0 (min start len))
                    (max 0 (min end len)))
            'list)))

(defun user-messages-tail (channel-id count)
  "Return the last COUNT messages from the per-user buffer."
  (let* ((buf (sync-user-chat-buffer channel-id))
         (len (length buf))
         (start (max 0 (- len count))))
    (coerce (subseq buf start len) 'list)))

;;; Private messages

(defun insert-message-sorted (buf msg)
  "Insert MSG into BUF at the correct chronological position.
Scans from the end since new messages typically belong near the tail."
  (let ((ts (getf msg :created-at))
        (len (length buf)))
    (vector-push-extend nil buf)
    (let ((pos (1- len)))
      (loop while (and (>= pos 0)
                       (let ((existing-ts (getf (aref buf pos) :created-at)))
                         (and existing-ts (> existing-ts ts))))
            do (setf (aref buf (1+ pos)) (aref buf pos))
               (decf pos))
      (setf (aref buf (1+ pos)) msg))))

(defun deliver-private-message (from-user to-username message)
  "Deliver a private message to a user's session. Returns T if delivered.
The message is inserted into the recipient's per-user chat buffer.
Sets the PM flag in the chat indicator if the recipient is not viewing latest chat."
  (let ((delivered (cons nil nil))
        (chat-count (count-chat-users)))
    (lispf:broadcast
     (lambda ()
       (let ((user (session-user lispf:*session*)))
         (when (and user (string-equal (user-username user) to-username))
           (let ((msg (list :username (user-username from-user)
                            :message message
                            :created-at (get-universal-time)
                            :private t)))
             (insert-message-sorted (user-chat-buffer) msg))
           (when (or (not (eq (lispf:session-current-screen lispf:*session*) 'chat))
                     (lispf:session-property lispf:*session* :chat-scroll-offset))
             (setf (lispf:session-property lispf:*session* :chat-pm-pending) t)
             (lispf:set-indicator "chat" (format-chat-indicator chat-count t)))
           (setf (car delivered) t)))))
    (car delivered)))


(defun send-own-private-message (from-user to-username message raw-input)
  "Add a sent private message to the sender's own buffer."
  (declare (ignore from-user))
  (let ((msg (list :username to-username
                   :message message
                   :created-at (get-universal-time)
                   :private t
                   :own t
                   :raw-input raw-input)))
    (vector-push-extend msg (user-chat-buffer))))
