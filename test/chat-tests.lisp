;;; -*- Mode: Lisp -*-

;;; E2E tests for chat.

(in-package #:veron-tests)

;;; Verify that a database error during chat entry does not disconnect the user.

(define-test e2e-chat-db-error-no-disconnect ()
  (with-veron-app (s :username "chatuser" :password "chatpass")
    (login s "chatuser" "chatpass")
    ;; Break the chat_messages table
    (veron::with-db
      (pomo:execute "ALTER TABLE chat_messages RENAME TO chat_messages_broken"))
    (unwind-protect
         (progn
           ;; Try to navigate to chat - will fail due to broken DB
           (move-cursor s 21 14)
           (erase-eof s)
           (type-text s "chat")
           (press-enter s)
           ;; The framework error handler restores to the last successful screen
           (assert-on-screen s "MAIN"))
      ;; Restore the table
      (veron::with-db
        (pomo:execute "ALTER TABLE chat_messages_broken RENAME TO chat_messages")))))

;;; Verify join/leave messages are persisted and displayed as notifications.

(define-test e2e-chat-join-leave-persisted ()
  (with-veron-app (s :username "chatuser" :password "chatpass")
    (login s "chatuser" "chatpass")
    ;; Enter chat, send a message, leave
    (navigate-to s "CHAT")
    (assert-screen-contains s "chatuser hat den Chat betreten")
    (type-text s "Hallo Welt")
    (press-enter s)
    (press-key s :pf3)
    (assert-on-screen s "MAIN")
    ;; Re-enter - join, message, and leave should all be visible
    (navigate-to s "CHAT")
    (assert-screen-contains s "chatuser hat den Chat betreten"
                            :description "Join message should be persisted")
    (assert-screen-contains s "Hallo Welt"
                            :description "Sent message should be persisted")
    (assert-screen-contains s "chatuser hat den Chat verlassen"
                            :description "Leave message should be persisted")
    (press-key s :pf3)))

;;; Verify that join/leave messages loaded from DB display correctly
;;; (as notifications without username prefix, not as regular messages).

(define-test e2e-chat-join-leave-display-after-reload ()
  (with-veron-app (s :username "chatuser" :password "chatpass")
    (login s "chatuser" "chatpass")
    ;; Enter chat, send a message, leave (creates join + message + leave in DB)
    (navigate-to s "CHAT")
    (type-text s "test message")
    (press-enter s)
    (press-key s :pf3)
    (assert-on-screen s "MAIN")
    ;; Simulate server restart: reload chat from DB
    (veron::load-chat-from-db)
    ;; Re-enter chat - messages loaded from DB should display correctly
    (navigate-to s "CHAT")
    ;; The join message should NOT have a (username) prefix
    (let* ((rows (screen-text s))
           (full (format nil "~{~A~^~%~}" rows)))
      ;; Should NOT find "(chatuser) --- chatuser hat den Chat betreten"
      (assert (not (search "(chatuser) ---" full)) ()
              "Join/leave loaded from DB should not have username prefix")
      ;; Should find the notification without prefix
      (assert (search "--- chatuser hat den Chat betreten" full) ()
              "Join message should be displayed as notification"))
    (press-key s :pf3)))

;;; Verify that the chat divider line shows current users and updates dynamically.

(defun wait-for-divider (session expected-names &key (timeout 2.0) (interval 0.1))
  "Wait until the chat divider line contains all EXPECTED-NAMES.
Polls at INTERVAL seconds, gives up after TIMEOUT seconds."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      ;; Trigger a screen refresh by pressing enter with empty input
      (press-enter session)
      (let* ((rows (screen-text session))
             (full (format nil "~{~A~^~%~}" rows)))
        (when (every (lambda (name) (search name full)) expected-names)
          (return t)))
      (when (> (get-internal-real-time) deadline)
        (return nil))
      (sleep interval))))

(defun wait-for-divider-without (session excluded-names &key (timeout 2.0) (interval 0.1))
  "Wait until the chat divider line does NOT contain any of EXCLUDED-NAMES.
Polls at INTERVAL seconds, gives up after TIMEOUT seconds."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (press-enter session)
      (let* ((rows (screen-text session))
             (full (format nil "~{~A~^~%~}" rows)))
        (when (notany (lambda (name)
                        (search (concatenate 'string " " name ",") full))
                      excluded-names)
          ;; Also check without comma (last/only name before " ---")
          (when (notany (lambda (name)
                          (search (concatenate 'string " " name " ---") full))
                        excluded-names)
            (return t))))
      (when (> (get-internal-real-time) deadline)
        (return nil))
      (sleep interval))))

(define-test e2e-chat-divider-shows-users ()
  (with-veron-app (s1 :username "hans" :password "hans")
    (login s1 "hans" "hans")
    ;; Hans joins - divider should show "hans"
    (navigate-to s1 "CHAT")
    (assert (wait-for-divider s1 '("hans")) ()
            "Divider should show hans after joining")
    (with-secondary-session (s2 :username "klaus" :password "klaus")
      (login s2 "klaus" "klaus")
      ;; Klaus joins - both dividers should show "hans, klaus"
      (navigate-to s2 "CHAT")
      (assert (wait-for-divider s1 '("hans" "klaus")) ()
              "Hans's divider should show both users after klaus joins")
      (assert (wait-for-divider s2 '("hans" "klaus")) ()
              "Klaus's divider should show both users after joining")
      ;; Hans leaves - klaus's divider should show only "klaus"
      (press-key s1 :pf3)
      (assert-on-screen s1 "MAIN")
      (assert (wait-for-divider-without s2 '("hans")) ()
              "Klaus's divider should not show hans after hans leaves")
      ;; Hans re-joins - both dividers should show "hans, klaus"
      (navigate-to s1 "CHAT")
      (assert (wait-for-divider s1 '("hans" "klaus")) ()
              "Hans's divider should show both users after re-joining")
      (assert (wait-for-divider s2 '("hans" "klaus")) ()
              "Klaus's divider should show both users after hans re-joins")
      ;; Klaus leaves - hans's divider should show only "hans"
      (press-key s2 :pf3)
      (assert-on-screen s2 "MAIN")
      (assert (wait-for-divider-without s1 '("klaus")) ()
              "Hans's divider should not show klaus after klaus leaves"))
    (press-key s1 :pf3)))

;;; Verify that two input lines are joined correctly.
;;; Short line1 + line2 should be joined with a single space.
;;; Full-width line1 + line2 should be concatenated directly.

(define-test e2e-chat-input-lines-joined-with-space ()
  (with-veron-app (s :username "chatuser" :password "chatpass")
    (login s "chatuser" "chatpass")
    (navigate-to s "CHAT")
    ;; Type short text in line1, then tab to line2 and type more
    (type-text s "hello")
    (tab-forward s)
    (type-text s "world")
    (press-enter s)
    ;; The message should appear as "hello world" (joined with space)
    (assert-screen-contains s "hello world"
                            :description "Short input lines should be joined with a space")
    (press-key s :pf3)))

(define-test e2e-chat-input-full-line-no-extra-space ()
  (with-veron-app (s :username "chatuser" :password "chatpass")
    (login s "chatuser" "chatpass")
    (navigate-to s "CHAT")
    ;; Fill line1 completely (79 usable chars) so last char is non-space
    (let ((long-text (make-string 79 :initial-element #\x)))
      (type-text s long-text)
      (tab-forward s)
      (type-text s "end")
      (press-enter s)
      ;; Should be concatenated directly without extra space
      (assert-screen-contains s "end"
                              :description "Continuation should appear")
      (let* ((rows (screen-text s))
             (full (format nil "~{~A~^~%~}" rows)))
        ;; Should NOT have "x end" (with space before "end")
        (assert (not (search "x end" full)) ()
                "Full-width line should not add extra space before continuation")))
    (press-key s :pf3)))

;;; Verify that a second user sees join/leave messages from the first user.

(define-test e2e-chat-two-users-see-join-leave ()
  (with-veron-app (s1 :username "alice" :password "alice")
    (login s1 "alice" "alice")
    (navigate-to s1 "CHAT")
    (with-secondary-session (s2 :username "bob" :password "bob")
      (login s2 "bob" "bob")
      (navigate-to s2 "CHAT")
      ;; Alice should see bob's join
      (sleep 0.5)
      (press-enter s1)
      (assert-screen-contains s1 "bob hat den Chat betreten"
                              :description "Alice should see Bob's join")
      ;; Bob sends a message
      (type-text s2 "Hello from Bob")
      (press-enter s2)
      ;; Alice should see the message
      (sleep 0.5)
      (press-enter s1)
      (assert-screen-contains s1 "Hello from Bob"
                              :description "Alice should see Bob's message")
      ;; Bob leaves
      (press-key s2 :pf3)
      (assert-on-screen s2 "MAIN")
      ;; Alice should see bob's leave
      (sleep 0.5)
      (press-enter s1)
      (assert-screen-contains s1 "bob hat den Chat verlassen"
                              :description "Alice should see Bob's leave"))))
