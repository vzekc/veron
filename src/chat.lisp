;;; -*- Mode: Lisp -*-

;;; Chat system - multi-channel persistent chat with private messaging

(in-package #:veron)

;;; Channel management

(defun default-channel-id ()
  "Return the ID of the default chat channel."
  (with-db
    (pomo:query "SELECT id FROM chat_channels WHERE name = 'allgemein'" :single)))

;;; Message persistence

(defun add-chat-message (channel-id user message)
  "Insert a chat message. Returns the new message ID."
  (with-db
    (pomo:query
     "INSERT INTO chat_messages (channel_id, user_id, username, message)
      VALUES ($1, $2, $3, $4) RETURNING id"
     channel-id (user-id user) (user-username user) message :single)))

(defun chat-messages-before (channel-id before-id count)
  "Load COUNT messages before BEFORE-ID (exclusive). Returns plists, oldest first."
  (with-db
    (let ((rows (pomo:query
                 "SELECT id, username, message, created_at FROM chat_messages
                  WHERE channel_id = $1 AND id < $2
                  ORDER BY id DESC LIMIT $3"
                 channel-id before-id count :plists)))
      (nreverse rows))))

(defun chat-messages-latest (channel-id count)
  "Load the latest COUNT messages. Returns plists, oldest first."
  (with-db
    (let ((rows (pomo:query
                 "SELECT id, username, message, created_at FROM chat_messages
                  WHERE channel_id = $1
                  ORDER BY id DESC LIMIT $2"
                 channel-id count :plists)))
      (nreverse rows))))

(defun chat-messages-after (channel-id after-id count)
  "Load COUNT messages after AFTER-ID (exclusive). Returns plists, oldest first."
  (with-db
    (pomo:query
     "SELECT id, username, message, created_at FROM chat_messages
      WHERE channel_id = $1 AND id > $2
      ORDER BY id ASC LIMIT $3"
     channel-id after-id count :plists)))

(defun chat-newest-id (channel-id)
  "Return the ID of the newest message in a channel, or 0."
  (with-db
    (or (pomo:query
         "SELECT MAX(id) FROM chat_messages WHERE channel_id = $1"
         channel-id :single)
        0)))

;;; Message formatting

(defconstant +chat-display-lines+ 17
  "Number of lines available for chat message display (rows 1-17).")

(defun format-time (universal-time)
  "Format a universal time as HH:MM."
  (if (or (null universal-time) (db-null-p universal-time))
      "     "
      (multiple-value-bind (sec min hour)
          (decode-universal-time universal-time)
        (declare (ignore sec))
        (format nil "~2,'0D:~2,'0D" hour min))))

(defun message-date (universal-time)
  "Return the date portion of a universal time as (day month year)."
  (when (and universal-time (not (db-null-p universal-time)))
    (multiple-value-bind (sec min hour day month year)
        (decode-universal-time universal-time)
      (declare (ignore sec min hour))
      (list day month year))))

(defun format-date-divider (day month year)
  "Format a date divider line."
  (format nil "--- ~2,'0D.~2,'0D.~4D ~53,,,'-A" day month year ""))

(defun wrap-message-lines (username message timestamp)
  "Format a chat message into display lines (max 79 chars each).
Returns a list of strings."
  (let* ((time-str (format-time timestamp))
         (prefix (format nil "~A ~A: " time-str username))
         (prefix-len (length prefix))
         (cont-indent (make-string (min prefix-len 20) :initial-element #\Space))
         (first-width (- 79 prefix-len))
         (rest-width (- 79 (length cont-indent)))
         (lines '())
         (text-lines (split-sequence:split-sequence #\Newline message)))
    (dolist (text-line text-lines)
      (if (null lines)
          ;; First line of first text-line
          (if (<= (length text-line) first-width)
              (push (concatenate 'string prefix text-line) lines)
              (progn
                (push (concatenate 'string prefix (subseq text-line 0 first-width)) lines)
                (loop for pos from first-width below (length text-line) by rest-width
                      do (push (concatenate 'string cont-indent
                                            (subseq text-line pos
                                                    (min (+ pos rest-width)
                                                         (length text-line))))
                               lines))))
          ;; Continuation text-lines
          (if (<= (length text-line) rest-width)
              (push (concatenate 'string cont-indent text-line) lines)
              (loop for pos from 0 below (length text-line) by rest-width
                    do (push (concatenate 'string cont-indent
                                          (subseq text-line pos
                                                  (min (+ pos rest-width)
                                                       (length text-line))))
                             lines)))))
    (nreverse lines)))

(defun format-chat-messages (messages)
  "Format a list of message plists into display lines with date dividers.
Returns a list of strings or plists (for colored lines)."
  (let ((lines '())
        (last-date nil))
    (dolist (msg messages)
      (let* ((timestamp (getf msg :created-at))
             (date (message-date timestamp)))
        (when (and date (not (equal date last-date)))
          (push (list :content (format-date-divider (first date) (second date) (third date))
                      :color cl3270:+turquoise+)
                lines)
          (setf last-date date))
        (dolist (line (wrap-message-lines (getf msg :username)
                                          (getf msg :message)
                                          timestamp))
          (push line lines))))
    (nreverse lines)))

;;; Private messages

(defun deliver-private-message (from-user to-username message)
  "Deliver a private message to a user's session. Returns T if delivered."
  (let ((delivered nil))
    (lspf:broadcast
     (lambda ()
       (let ((user (session-user lspf:*session*)))
         (when (and user (string-equal (user-username user) to-username))
           (let ((inbox (or (lspf:session-property lspf:*session* :chat-inbox) '())))
             (setf (lspf:session-property lspf:*session* :chat-inbox)
                   (append inbox
                           (list (list :username (format nil "~A (privat)" (user-username from-user))
                                       :message message
                                       :created-at (get-universal-time))))))
           (setf delivered t)))))
    delivered))

(defun consume-private-messages ()
  "Return and clear pending private messages for the current session."
  (let ((msgs (lspf:session-property lspf:*session* :chat-inbox)))
    (when msgs
      (setf (lspf:session-property lspf:*session* :chat-inbox) nil))
    msgs))
