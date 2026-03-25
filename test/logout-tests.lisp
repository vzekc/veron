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

;;; Normal logout via PF3 + PF5 should record logout time.

(define-test e2e-logout-normal-records-time ()
  (with-veron-app (s :username "loguser" :password "logpass")
    (login s "loguser" "logpass")
    ;; Logout normally
    (press-key s :pf3)
    (assert-on-screen s "LOGOUT")
    (press-key s :pf5)
    ;; Session should disconnect - wait for session thread to finish
    (sleep 0.5)
    ;; Check the login record has a logout timestamp
    (assert (last-login-has-logout-p) ()
            "Normal logout should record logout_at")))

;;; Connection drop (client disconnect) should also record logout time.

(define-test e2e-logout-connection-drop-records-time ()
  (with-veron-app (s :username "dropuser" :password "droppass")
    (login s "dropuser" "droppass")
    ;; Forcibly disconnect without logging out
    (s3270-disconnect s)
    (close-s3270 s)
    ;; Wait for session thread to finish cleanup
    (sleep 0.5)
    ;; Check the login record has a logout timestamp
    (assert (last-login-has-logout-p) ()
            "Connection drop should record logout_at")))
