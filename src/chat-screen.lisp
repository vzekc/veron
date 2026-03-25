;;; -*- Mode: Lisp -*-

;;; Chat screen handlers - UI layer for the chat system.

(in-package #:veron)

;;; Chat indicators

(defun count-chat-users ()
  "Count sessions currently on the chat screen."
  (length (chat-user-names)))

(defun format-chat-indicator (user-count pm-pending)
  "Format combined chat indicator: CHAT:xxi where xx=users, i=PM flag."
  (format nil "CHAT:~2,'0D~A" (min user-count 99) (if pm-pending "*" " ")))

(defun update-chat-indicators ()
  "Update chat indicator on all sessions with current count and per-session PM flag."
  (let ((count (count-chat-users)))
    (lspf:broadcast
     (lambda ()
       (lspf:set-indicator "chat"
         (format-chat-indicator count
           (lspf:session-property lspf:*session* :chat-pm-pending)))))))

(defun update-my-chat-indicator ()
  "Update just the current session's chat indicator."
  (lspf:set-indicator "chat"
    (format-chat-indicator (count-chat-users)
      (lspf:session-property lspf:*session* :chat-pm-pending))))

;;; Chat screen

(defun chat-user-names ()
  "Return a sorted list of usernames currently in the chat."
  (let ((names '()))
    (bt:with-lock-held ((lspf::application-sessions-lock lspf:*application*))
      (dolist (s (lspf::application-sessions lspf:*application*))
        (when (and (eq (lspf:session-current-screen s) 'chat)
                   (not (lspf:session-property s :chat-leaving)))
          (let ((user (session-user s)))
            (when user (push (user-username user) names))))))
    (sort names #'string-lessp)))

(defun format-chat-divider ()
  "Format the chat divider line showing usernames of users in chat.
Right-aligned: dashes on the left, then user names, then ' ---'.
Elides with '...' if names don't fit."
  (let* ((names (chat-user-names))
         (suffix " ---")
         (available (- 80 (length suffix) 1))
         (name-str (format nil "~{~A~^, ~}" names)))
    (when (> (length name-str) available)
      (setf name-str
            (loop with result = ""
                  for name in names
                  for sep = "" then ", "
                  for candidate = (concatenate 'string result sep name)
                  while (<= (+ (length candidate) 4) available)
                  do (setf result candidate)
                  finally (return (concatenate 'string result sep "...")))))
    (let* ((content (concatenate 'string " " name-str suffix))
           (pad-len (- 80 (length content))))
      (concatenate 'string (make-string (max 0 pad-len) :initial-element #\-) content))))

(lspf:define-screen-update chat ()
  (lspf:set-cursor 20 0)
  (setf (lspf:session-property lspf:*session* :chat-leaving) nil)
  (let ((entering (not (lspf:session-property lspf:*session* :chat-entered))))
    (unless (lspf:session-property lspf:*session* :chat-entered)
      (setf (lspf:session-property lspf:*session* :chat-entered) t
            (lspf:session-property lspf:*session* :chat-loading) t
            (lspf:session-property lspf:*session* :chat-user-buffer) nil
            (lspf:session-property lspf:*session* :chat-sync-index) 0
            (lspf:session-property lspf:*session* :chat-sent-ids) nil))
    (update-chat-indicators)
    (when entering
      (let ((name (current-username)))
        (when name
          (add-chat-notification
           (chat-channel-id)
           (session-user lspf:*session*)
           "--- ~A hat den Chat betreten" name)))))
  (let ((total (length (chat-all-formatted-lines))))
    (when (> total +chat-display-lines+)
      (lspf:show-key :pf7 "Aeltere"))
    (when (chat-scroll-offset)
      (lspf:show-key :pf6 "Neueste")
      (lspf:show-key :pf8 "Neuere"))))

(defun chat-channel-id ()
  "Return the chat channel ID for the current session, initializing if needed."
  (or (lspf:session-property lspf:*session* :chat-channel-id)
      (let ((id (default-channel-id)))
        (setf (lspf:session-property lspf:*session* :chat-channel-id) id)
        id)))

(defun chat-scroll-offset ()
  "Return the scroll offset (end line index), or NIL for latest."
  (lspf:session-property lspf:*session* :chat-scroll-offset))

(defun chat-all-formatted-lines ()
  "Format all messages in the per-user buffer into display lines.
Returns the full list of formatted lines."
  (let* ((channel-id (chat-channel-id))
         (msgs (coerce (sync-user-chat-buffer channel-id) 'list)))
    (format-chat-messages msgs :start-of-log t)))

(defun current-username ()
  "Return the current session's username."
  (let ((user (session-user lspf:*session*)))
    (when user (user-username user))))

(defun chat-divider-line ()
  "Return the divider line as a colored plist for the dynamic area."
  (list :content (format-chat-divider) :color cl3270:+yellow+))

(lspf:define-dynamic-area-updater chat messages ()
  `(,@(let* ((all-lines (chat-all-formatted-lines))
             (total (length all-lines))
             (offset (chat-scroll-offset)))
        (when (and (not offset)
                   (lspf:session-property lspf:*session* :chat-pm-pending))
          (setf (lspf:session-property lspf:*session* :chat-pm-pending) nil)
          (update-my-chat-indicator))
        (cond
          ;; Scrolled back: show page ending at offset
          (offset
           (let* ((end (min offset total))
                  (start (max 0 (- end +chat-display-lines+)))
                  (page (subseq all-lines start end))
                  (n (length page)))
             (if (< n +chat-display-lines+)
                 (append page
                         (make-list (- +chat-display-lines+ n) :initial-element ""))
                 page)))
          ;; Live view: bottom-align last page
          ((<= total +chat-display-lines+)
           (append (make-list (- +chat-display-lines+ total) :initial-element "")
                   all-lines))
          (t (last all-lines +chat-display-lines+))))
    ,(chat-divider-line)))

;;; Chat input handling

(defvar *chat-input-field-length* 79
  "Usable length of a chat input field (field length minus attribute byte).")

(defun parse-chat-input (input1 input2)
  "Combine two input lines into a single message string.
If line1 completely fills the field, concatenate directly (the user's text
runs right up to the boundary). Otherwise join with a single space."
  (concatenate 'string
               input1
               (if (< (length input1) *chat-input-field-length*)
                   " "
                   "")
               input2))

(defun handle-chat-leave ()
  "Handle leaving the chat. Always persists a leave message."
  (let ((name (current-username))
        (channel-id (chat-channel-id)))
    (setf (lspf:session-property lspf:*session* :chat-leaving) t
          (lspf:session-property lspf:*session* :chat-entered) nil)
    (update-chat-indicators)
    (when name
      (add-chat-notification channel-id
                             (session-user lspf:*session*)
                             "--- ~A hat den Chat verlassen" name))))

(defun clear-chat-input ()
  "Clear the chat input fields for no-clear redisplay."
  (let ((blank (make-string 80 :initial-element #\Space))
        (context (lspf:session-context lspf:*session*)))
    (setf (gethash "input1" context) blank
          (gethash "input2" context) blank)))

(lspf:define-key-handler chat :enter (input1 input2)
  (let ((text (parse-chat-input (or input1 "") (or input2 ""))))
    (clear-chat-input)
    (when (string= (string-trim '(#\Space) text) "")
      (return-from lspf:handle-key :stay))
    ;; Handle /who command
    (when (string-equal (string-trim '(#\Space) text) "/who")
      (vector-push-extend
       (make-chat-message :notification "--- Im Chat: ~{~A~^, ~}"
                          (chat-user-names))
       (user-chat-buffer))
      (return-from lspf:handle-key :stay))
    ;; Handle /quit command
    (when (string-equal (string-trim '(#\Space) text) "/quit")
      (handle-chat-leave)
      (return-from lspf:handle-key :back))
    ;; Handle /msg command
    (when (and (>= (length text) 5)
               (string-equal (subseq text 0 4) "/msg"))
      (let* ((rest (string-trim '(#\Space) (subseq text 4)))
             (space-pos (position #\Space rest)))
        (unless space-pos
          (lspf:application-error "/msg <Name> <Nachricht>"))
        (let ((recipient (subseq rest 0 space-pos))
              (msg (string-trim '(#\Space) (subseq rest (1+ space-pos)))))
          (when (string= msg "")
            (lspf:application-error "/msg <Name> <Nachricht>"))
          (let ((user (session-user lspf:*session*)))
            (unless (deliver-private-message user recipient msg)
              (lspf:application-error
               (format nil "~A ist nicht online" recipient)))
            (send-own-private-message user recipient msg text))
          (return-from lspf:handle-key :stay))))
    ;; Unknown / command
    (when (and (plusp (length text)) (char= (char text 0) #\/))
      (let* ((cmd (string-trim '(#\Space) text))
             (end (or (position #\Space cmd) (length cmd))))
        (vector-push-extend
         (make-chat-message :notification "--- Unbekannter Befehl: ~A" (subseq cmd 0 end))
         (user-chat-buffer)))
      (return-from lspf:handle-key :stay))
    ;; Regular message — add locally and to shared buffer
    (let ((user (session-user lspf:*session*)))
      (add-own-message (chat-channel-id) user text text))
    (setf (lspf:session-property lspf:*session* :chat-scroll-offset) nil))
  :stay)

(lspf:define-key-handler chat :pf3 ()
  (handle-chat-leave)
  :back)

(lspf:define-key-handler chat :pf6 ()
  (setf (lspf:session-property lspf:*session* :chat-scroll-offset) nil)
  :stay)

(lspf:define-key-handler chat :pf7 ()
  (let* ((all-lines (chat-all-formatted-lines))
         (total (length all-lines))
         (offset (or (chat-scroll-offset) total))
         (new-offset (max +chat-display-lines+ (- offset +chat-display-lines+))))
    (when (< new-offset offset)
      (setf (lspf:session-property lspf:*session* :chat-scroll-offset)
            new-offset)))
  :stay)

(lspf:define-key-handler chat :pf8 ()
  (let ((offset (chat-scroll-offset)))
    (unless offset
      (return-from lspf:handle-key :stay))
    (let* ((all-lines (chat-all-formatted-lines))
           (total (length all-lines))
           (new-offset (+ offset +chat-display-lines+)))
      (if (>= new-offset total)
          (setf (lspf:session-property lspf:*session* :chat-scroll-offset) nil)
          (setf (lspf:session-property lspf:*session* :chat-scroll-offset)
                new-offset))))
  :stay)
