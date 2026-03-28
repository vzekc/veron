;;; -*- Mode: Lisp -*-

;;; E2E tests for LU configuration admin screens.

(in-package #:veron-tests)

;;; Full lifecycle: list, create, edit, delete

(define-test e2e-lu-config-lifecycle ()
  (with-veron-app (s :username "luadmin" :password "luadminpass"
                     :roles '(:veron-administrator))
    (login s "luadmin" "luadminpass")
    (assert-on-screen s "MAIN")
    ;; Navigate to System > LU Konfig
    (navigate-to s "LU-CONFIG")
    (assert-on-screen s "LU-CONFIG")
    ;; Default * entry should be visible
    (assert-screen-contains s "*")
    ;; Create a new LU
    (press-pf s 5)
    (assert-on-screen s "LU-CONFIG-NEW")
    ;; Fields: lu-name at content (3,22) = physical (4,23)
    (move-cursor s 4 23)
    (type-text s "TESTLU01")
    (press-pf s 5)
    (assert-on-screen s "LU-CONFIG")
    (assert-screen-contains s "TESTLU01")
    ;; Edit the new LU — it's the second entry (row 4 on 24-row, row 4 on 43-row)
    (move-cursor s 4 5)
    (press-enter s)
    (assert-on-screen s "LU-CONFIG-EDIT")
    (assert-screen-contains s "TESTLU01")
    ;; Change description
    (move-cursor s 5 23)
    (erase-eof s)
    (type-text s "Updated desc")
    (press-pf s 5)
    (assert-screen-contains s "Gespeichert")
    ;; Go back to list
    (press-pf s 3)
    (assert-on-screen s "LU-CONFIG")
    ;; Edit again and delete — find row again
    (let ((row (loop for r from 3 to 19
                     when (search "TESTLU01" (screen-row s r))
                       return r)))
      (assert row () "TESTLU01 not found for delete")
      (move-cursor s row 5)
      (press-enter s))
    (assert-on-screen s "LU-CONFIG-EDIT")
    (press-pf s 9)
    (assert-screen-contains s "loeschen")
    (press-pf s 5)
    (assert (wait-for-screen-contains s "geloescht" :timeout 3)
            () "Should see delete confirmation message")
    ;; Verify the LU is not in the data rows (row 3+), ignoring the message line (row 22)
    (let ((found (loop for r from 3 to 19
                       thereis (search "TESTLU01" (screen-row s r)))))
      (assert (not found) () "Deleted LU should not appear in list data"))))

;;; TODO: test that connection with unknown LU name is rejected
;;; TODO: test that connection from wrong IP for a configured LU is rejected

;;; Default LU (*) cannot be deleted

(define-test e2e-lu-config-default-not-deletable ()
  (with-veron-app (s :username "luadmin2" :password "luadmin2pass"
                     :roles '(:veron-administrator))
    (login s "luadmin2" "luadmin2pass")
    (navigate-to s "LU-CONFIG")
    (assert-on-screen s "LU-CONFIG")
    ;; Select the * entry
    (move-cursor s 3 5)
    (press-enter s)
    (assert-on-screen s "LU-CONFIG-EDIT")
    (assert-screen-contains s "*")
    ;; PF9 should show error about default
    (press-pf s 9)
    (assert-screen-contains s "Standard LU")))
