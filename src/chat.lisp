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

(defun format-silence-divider (gap-seconds)
  "Format a silence divider showing the duration in German words."
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
    (format nil "-- nach ~{~A~^, ~} Schweigen:" parts)))

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

(defconstant +silence-threshold+ (* 15 60)
  "Seconds of silence before inserting a timestamp divider.")

(defun format-chat-messages (messages &optional current-username)
  "Format a list of message plists into display lines.
Inserts a timestamp divider after 15+ minutes of silence.
When CURRENT-USERNAME is given, highlight that user's messages.
Returns a list of strings or plists (for colored lines)."
  (let ((lines '())
        (last-time nil))
    (dolist (msg messages)
      (let* ((timestamp (getf msg :created-at))
             (username (getf msg :username))
             (private-p (getf msg :private))
             (own-p (and current-username (not private-p)
                         (string-equal username current-username))))
        ;; Insert silence divider if gap between public messages > 15 minutes
        (when (and (not private-p) last-time timestamp
                   (> (- timestamp last-time) +silence-threshold+))
          (push (list :content (format-silence-divider (- timestamp last-time))
                      :color cl3270:+turquoise+)
                lines))
        (when (and timestamp (not private-p))
          (setf last-time timestamp))
        (dolist (line (wrap-message-lines username (getf msg :message)
                                          :private private-p))
          (push (cond (private-p (list :content line :color cl3270:+yellow+))
                      (own-p (list :content line :color cl3270:+white+))
                      (t line))
                lines))))
    (nreverse lines)))

;;; Per-user message buffer
;;;
;;; Each session maintains its own message buffer that merges public
;;; messages from the shared channel buffer with private messages.
;;; The buffer is lazily populated from the shared buffer and syncs
;;; new public messages on each access.

(defun user-chat-buffer ()
  "Return the per-user chat buffer for the current session, creating if needed."
  (or (lspf:session-property lspf:*session* :chat-user-buffer)
      (setf (lspf:session-property lspf:*session* :chat-user-buffer)
            (make-array 0 :adjustable t :fill-pointer 0))))

(defun sync-user-chat-buffer (channel-id)
  "Sync the per-user buffer with new public messages from the shared channel buffer.
Returns the per-user buffer."
  (let* ((buf (user-chat-buffer))
         (synced (or (lspf:session-property lspf:*session* :chat-sync-index) 0))
         (shared (channel-messages channel-id))
         (shared-len (length shared)))
    (when (< synced shared-len)
      (loop for i from synced below shared-len
            do (vector-push-extend (aref shared i) buf))
      (setf (lspf:session-property lspf:*session* :chat-sync-index) shared-len))
    buf))

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
    ;; Shift elements right until we find the insertion point
    (let ((pos (1- len)))
      (loop while (and (>= pos 0)
                       (let ((existing-ts (getf (aref buf pos) :created-at)))
                         (and existing-ts (> existing-ts ts))))
            do (setf (aref buf (1+ pos)) (aref buf pos))
               (decf pos))
      (setf (aref buf (1+ pos)) msg))))

(defun deliver-private-message (from-user to-username message)
  "Deliver a private message to a user's session. Returns T if delivered.
The message is inserted into the recipient's per-user chat buffer in
chronological order."
  (let ((delivered (cons nil nil)))
    (lspf:broadcast
     (lambda ()
       (let ((user (session-user lspf:*session*)))
         (when (and user (string-equal (user-username user) to-username))
           (let ((msg (list :username (user-username from-user)
                            :message message
                            :created-at (get-universal-time)
                            :private t)))
             (insert-message-sorted (user-chat-buffer) msg))
           (setf (car delivered) t)))))
    (car delivered)))
