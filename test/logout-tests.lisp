;;; -*- Mode: Lisp -*-

;;; E2E tests for logout tracking.

(in-package #:veron-tests)

(defun last-login-has-logout-p ()
  "Check if the most recent login record has a logout timestamp."
  (veron::with-db
    (let ((row (pomo:query
                "SELECT logout_at FROM logins ORDER BY id DESC LIMIT 1"
                :row)))
      (and row (not (veron::db-null-p (first row)))))))

;;; Normal logout via PF3 (confirm) + PF5 should record logout time.

(define-test e2e-logout-normal-records-time ()
  (with-test-db (db-name)
    (create-test-user "loguser" "logpass")
    (veron::load-chat-from-db)
    (with-test-app (s veron::*veron-app*)
      (login s "loguser" "logpass")
      ;; PF3 on MAIN triggers confirmation prompt (stays on MAIN)
      (press-key s :pf3)
      (assert-on-screen s "MAIN")
      ;; PF5 confirms logout → goes to GOODBYE
      (press-key s :pf5)
      ;; Disconnect client from goodbye screen
      (ignore-errors (s3270-disconnect s))
      (ignore-errors (close-s3270 s)))
    ;; Session has drained; test DB still alive - check logout was recorded
    (assert (last-login-has-logout-p) ()
            "Normal logout should record logout_at")))

;;; Logout command from command field triggers confirmation

(define-test e2e-logout-command ()
  (with-veron-app (s :username "cmdloguser" :password "cmdlogpass")
    (enable-default-disconnect)
    (login s "cmdloguser" "cmdlogpass")
    (assert-on-screen s "MAIN")
    ;; Type "logout" in the command field
    (move-cursor s 21 14)
    (erase-eof s)
    (type-text s "logout")
    (press-enter s)
    ;; Should show confirmation
    (assert-on-screen s "MAIN")
    (assert-screen-contains s "Wirklich abmelden?")
    ;; Confirm
    (press-key s :pf5)
    (assert-on-screen s "GOODBYE")))

;;; Logout command works from a submenu screen

(define-test e2e-logout-command-from-submenu ()
  (with-veron-app (s :username "subloguser" :password "sublogpass")
    (enable-default-disconnect)
    (login s "subloguser" "sublogpass")
    (assert-on-screen s "MAIN")
    ;; Navigate to a submenu (guestbook)
    (navigate-to s "guestbook")
    (assert-on-screen s "GUESTBOOK")
    ;; Type "quit" in the command field
    (move-cursor s 21 14)
    (erase-eof s)
    (type-text s "quit")
    (press-enter s)
    ;; Should show confirmation
    (assert-screen-contains s "Wirklich abmelden?")
    ;; Confirm
    (press-key s :pf5)
    (assert-on-screen s "GOODBYE")))

;;; Goodbye screen: entire farewell message has turquoise color attribute

(define-test e2e-goodbye-farewell-color ()
  (with-veron-app (s :username "coloruser" :password "colorpass")
    (enable-default-disconnect)
    (login s "coloruser" "colorpass")
    (assert-on-screen s "MAIN")
    (press-key s :pf3)
    (assert-on-screen s "MAIN")
    (press-key s :pf5)
    (assert-on-screen s "GOODBYE")
    ;; Row 19 = "Vielen Dank ... Mal!" line (title at row 0, content row 18)
    ;; Check that "Mal!" is inside the turquoise field, not after an SF reset.
    ;; In the buffer, the hex for "n Mal!" (end of "naechsten Mal!") must be
    ;; contiguous without any SF(...) marker splitting the text.
    (let* ((buffer-rows (read-buffer s))
           (row (nth 19 buffer-rows)))
      (assert (not (cl-ppcre:scan "6e SF\\(" row)) ()
              "Goodbye farewell message: 'Mal!' must be inside turquoise field, ~
               but found SF marker after 'naechsten'"))))

;;; Connection drop (client disconnect) should also record logout time.

(define-test e2e-logout-connection-drop-records-time ()
  (with-test-db (db-name)
    (create-test-user "dropuser" "droppass")
    (veron::load-chat-from-db)
    (with-test-app (s veron::*veron-app*)
      (login s "dropuser" "droppass")
      (s3270-disconnect s)
      (close-s3270 s))
    ;; Session has drained; test DB still alive - check logout was recorded
    (assert (last-login-has-logout-p) ()
            "Connection drop should record logout_at")))
