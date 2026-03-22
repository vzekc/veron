;;; -*- Mode: Lisp -*-

;;; Chat system - multi-channel persistent chat with private messaging
;;;
;;; Messages are kept in RAM for fast access. The database is used for
;;; persistence and recovery only. On startup, messages are loaded from
;;; the DB into the in-memory buffer.

(in-package #:veron)

;;; In-memory message store

(defvar *chat-messages* (make-hash-table)
  "Channel ID -> vector of message plists, ordered oldest first.")

(defvar *chat-lock* (bt:make-lock "chat")
  "Lock protecting *chat-messages*.")

(defvar *chat-id-counter* 0
  "Monotonically increasing message ID for in-memory messages.")

(defun load-chat-from-db ()
  "Load all chat messages from the database into RAM."
  (with-db
    (let ((channels (pomo:query "SELECT id FROM chat_channels" :column)))
      (dolist (channel-id channels)
        (let ((msgs (pomo:query
                     "SELECT id, username, message, created_at FROM chat_messages
                      WHERE channel_id = $1 ORDER BY id ASC"
                     channel-id :plists)))
          (setf (gethash channel-id *chat-messages*)
                (make-array (length msgs)
                            :adjustable t :fill-pointer (length msgs)
                            :initial-contents msgs))
          (when msgs
            (setf *chat-id-counter*
                  (max *chat-id-counter*
                       (getf (car (last msgs)) :id)))))))))

(defun ensure-channel-buffer (channel-id)
  "Ensure a message buffer exists for CHANNEL-ID."
  (or (gethash channel-id *chat-messages*)
      (setf (gethash channel-id *chat-messages*)
            (make-array 0 :adjustable t :fill-pointer 0))))

(defun channel-messages (channel-id)
  "Return the message vector for CHANNEL-ID."
  (bt:with-lock-held (*chat-lock*)
    (ensure-channel-buffer channel-id)))

;;; Channel management

(defun default-channel-id ()
  "Return the ID of the default chat channel."
  (with-db
    (pomo:query "SELECT id FROM chat_channels WHERE name = 'allgemein'" :single)))

;;; Message operations

(defun add-chat-message (channel-id user message)
  "Add a chat message to RAM and persist to DB."
  (let* ((now (get-universal-time))
         (db-id (with-db
                  (pomo:query
                   "INSERT INTO chat_messages (channel_id, user_id, username, message)
                    VALUES ($1, $2, $3, $4) RETURNING id"
                   channel-id (user-id user) (user-username user) message :single)))
         (msg (list :id db-id
                    :username (user-username user)
                    :message message
                    :created-at now)))
    (bt:with-lock-held (*chat-lock*)
      (let ((buf (ensure-channel-buffer channel-id)))
        (vector-push-extend msg buf))
      (setf *chat-id-counter* (max *chat-id-counter* db-id)))
    db-id))

(defun chat-message-count (channel-id)
  "Return the number of messages in a channel."
  (length (channel-messages channel-id)))

(defun chat-messages-slice (channel-id start end)
  "Return messages from index START to END (exclusive) as a list."
  (let ((buf (channel-messages channel-id)))
    (when buf
      (let ((len (length buf)))
        (coerce (subseq buf (max 0 (min start len))
                        (max 0 (min end len)))
                'list)))))

(defun chat-messages-tail (channel-id count)
  "Return the last COUNT messages as a list, oldest first."
  (let* ((buf (channel-messages channel-id))
         (len (length buf))
         (start (max 0 (- len count))))
    (coerce (subseq buf start len) 'list)))

;;; Message formatting

(defconstant +chat-display-lines+ 17
  "Number of lines available for chat message display (rows 1-17).")

(defun format-time (universal-time)
  "Format a universal time as HH:MM in the display timezone."
  (if (or (null universal-time) (db-null-p universal-time))
      "     "
      (multiple-value-bind (sec min hour)
          (decode-display-time universal-time)
        (declare (ignore sec))
        (format nil "~2,'0D:~2,'0D" hour min))))

(defun message-date (universal-time)
  "Return the date portion of a universal time in the display timezone."
  (when (and universal-time (not (db-null-p universal-time)))
    (multiple-value-bind (sec min hour day month year)
        (decode-display-time universal-time)
      (declare (ignore sec min hour))
      (list day month year))))

(defun format-date-divider (day month year)
  "Format a date divider line."
  (format nil "--- ~2,'0D.~2,'0D.~4D ~53,,,'-A" day month year ""))

(defun word-wrap (text width)
  "Wrap TEXT to WIDTH characters, breaking at word boundaries when possible.
Returns a list of strings."
  (when (zerop (length text))
    (return-from word-wrap (list "")))
  (let ((lines '())
        (pos 0)
        (len (length text)))
    (loop while (< pos len)
          do (let ((remaining (- len pos)))
               (if (<= remaining width)
                   (progn
                     (push (subseq text pos) lines)
                     (setf pos len))
                   (let* ((end (+ pos width))
                          (break-pos (position #\Space text :end end :from-end t :start pos)))
                     (if (and break-pos (> break-pos pos))
                         (progn
                           (push (subseq text pos break-pos) lines)
                           (setf pos (1+ break-pos)))
                         (progn
                           (push (subseq text pos end) lines)
                           (setf pos end)))))))
    (nreverse lines)))

(defun wrap-message-lines (username message timestamp)
  "Format a chat message into display lines with word wrapping.
Returns a list of strings."
  (let* ((time-str (format-time timestamp))
         (prefix (format nil "~A ~A: " time-str username))
         (prefix-len (length prefix))
         (cont-indent (make-string (min prefix-len 20) :initial-element #\Space))
         (first-width (- 80 prefix-len))
         (rest-width (- 80 (length cont-indent)))
         (wrapped (word-wrap message first-width))
         (lines '()))
    ;; First wrapped line gets the prefix
    (push (concatenate 'string prefix (or (first wrapped) "")) lines)
    ;; Remaining wrapped lines get continuation indent
    (dolist (line (rest wrapped))
      ;; Re-wrap if continuation is longer than rest-width
      (if (<= (length line) rest-width)
          (push (concatenate 'string cont-indent line) lines)
          (dolist (sub (word-wrap line rest-width))
            (push (concatenate 'string cont-indent sub) lines))))
    (nreverse lines)))

(defun format-chat-messages (messages &optional current-username)
  "Format a list of message plists into display lines with date dividers.
When CURRENT-USERNAME is given, highlight that user's messages.
Returns a list of strings or plists (for colored lines)."
  (let ((lines '())
        (last-date nil))
    (dolist (msg messages)
      (let* ((timestamp (getf msg :created-at))
             (date (message-date timestamp))
             (username (getf msg :username))
             (own-p (and current-username
                         (string-equal username current-username))))
        (when (and date (not (equal date last-date)))
          (push (list :content (format-date-divider (first date) (second date) (third date))
                      :color cl3270:+turquoise+)
                lines)
          (setf last-date date))
        (dolist (line (wrap-message-lines username (getf msg :message) timestamp))
          (push (if own-p
                    (list :content line :color cl3270:+white+)
                    line)
                lines))))
    (nreverse lines)))

;;; Private messages

(defun deliver-private-message (from-user to-username message)
  "Deliver a private message to a user's session. Returns T if delivered."
  (let ((delivered (cons nil nil)))
    (lspf:broadcast
     (lambda ()
       (let ((user (session-user lspf:*session*)))
         (when (and user (string-equal (user-username user) to-username))
           (let ((inbox (or (lspf:session-property lspf:*session* :chat-inbox) '())))
             (setf (lspf:session-property lspf:*session* :chat-inbox)
                   (nconc inbox
                          (list (list :username (format nil "~A (privat)" (user-username from-user))
                                      :message message
                                      :created-at (get-universal-time))))))
           (setf (car delivered) t)))))
    (car delivered)))

(defun collect-private-messages ()
  "Move any new private messages from inbox to the visible list.
Returns the full visible private message list."
  (let ((inbox (lspf:session-property lspf:*session* :chat-inbox)))
    (when inbox
      (setf (lspf:session-property lspf:*session* :chat-inbox) nil)
      (setf (lspf:session-property lspf:*session* :chat-private-visible)
            (nconc (or (lspf:session-property lspf:*session* :chat-private-visible) '())
                   inbox)))
    (lspf:session-property lspf:*session* :chat-private-visible)))

(defun clear-private-messages ()
  "Clear the visible private message list."
  (setf (lspf:session-property lspf:*session* :chat-private-visible) nil))
