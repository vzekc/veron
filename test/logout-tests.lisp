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
