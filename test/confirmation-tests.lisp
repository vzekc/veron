;;; -*- Mode: Lisp -*-

;;; E2E tests for the framework confirmation dialog.

(in-package #:veron-tests)

;;; Logout confirmation: PF5 confirms → GOODBYE screen

(define-test e2e-confirmation-confirm-logout ()
  (with-veron-app (s :username "confloguser" :password "conflogpass")
    (enable-default-disconnect)
    (login s "confloguser" "conflogpass")
    (assert-on-screen s "MAIN")
    ;; PF3 triggers confirmation
    (press-key s :pf3)
    (assert-on-screen s "MAIN")
    (assert (wait-for-screen-contains s "Wirklich abmelden?" :timeout 2)
            () "Confirmation message should appear")
    (assert (wait-for-screen-contains s "Bestaetigen" :timeout 2)
            () "PF5 Bestaetigen label should be visible")
    ;; PF5 confirms logout → goes to GOODBYE
    (press-key s :pf5)
    (assert-on-screen s "GOODBYE")))

;;; Logout confirmation: PF3 cancels, restores normal screen

(define-test e2e-confirmation-cancel ()
  (with-veron-app (s :username "confuser" :password "confpass")
    (login s "confuser" "confpass")
    (assert-on-screen s "MAIN")
    ;; PF3 triggers confirmation
    (press-key s :pf3)
    (assert-on-screen s "MAIN")
    (assert (wait-for-screen-contains s "Wirklich abmelden?" :timeout 2)
            () "Confirmation message should be visible")
    (assert (wait-for-screen-contains s "Zurueck" :timeout 2)
            () "PF3 Zurueck label should be visible")
    (assert (not (wait-for-screen-contains s "Abmelden" :timeout 1))
            () "Normal PF3 label should be hidden during confirmation")
    ;; PF3 cancels — back to normal MAIN
    (press-key s :pf3)
    (assert-on-screen s "MAIN")
    (assert (not (wait-for-screen-contains s "Wirklich abmelden?" :timeout 1))
            () "Confirmation message should be gone after cancel")
    (assert (wait-for-screen-contains s "Abmelden" :timeout 2)
            () "Normal PF3 label should be restored after cancel")))

;;; Confirmation key labels are left-aligned

(define-test e2e-confirmation-keys-left-aligned ()
  (with-veron-app (s :username "alignuser" :password "alignpass")
    (login s "alignuser" "alignpass")
    (assert-on-screen s "MAIN")
    (press-key s :pf3)
    (assert (wait-for-screen-contains s "Wirklich abmelden?" :timeout 2))
    ;; Row 23 (0-based) should have key labels starting near column 1
    (let ((key-row (screen-row s 23)))
      (assert (and (>= (length key-row) 5)
                   (string= "PF3" (string-trim '(#\Space) (subseq key-row 0 4))))
              () "PF3 label should start at left of row 23, got: ~S"
              (subseq key-row 0 (min 40 (length key-row)))))))

;;; Other keys during confirmation are ignored

(define-test e2e-confirmation-other-key-ignored ()
  (with-veron-app (s :username "otheruser" :password "otherpass")
    (login s "otheruser" "otherpass")
    (assert-on-screen s "MAIN")
    ;; PF3 triggers confirmation
    (press-key s :pf3)
    (assert (wait-for-screen-contains s "Wirklich abmelden?" :timeout 2))
    ;; Press Enter — should stay in confirmation
    (press-enter s)
    (assert-on-screen s "MAIN")
    (assert (wait-for-screen-contains s "Wirklich abmelden?" :timeout 2)
            () "Confirmation should persist after other key")))
