;;; -*- Mode: Lisp -*-

;;; E2E tests for role-based access control.

(in-package #:veron-tests)

;;; Non-admin user is denied access to admin screen

(define-test e2e-role-denied-without-admin ()
  (with-veron-app (s :username "normaluser" :password "testpass")
    (login s "normaluser" "testpass")
    (assert-on-screen s "MAIN")
    ;; Navigate to system menu
    (move-cursor s 21 14)
    (erase-eof s)
    (type-text s "5")
    (press-enter s)
    (assert-on-screen s "SYSTEM")
    ;; Try to access users screen
    (move-cursor s 21 14)
    (erase-eof s)
    (type-text s "3")
    (press-enter s)
    ;; Should stay on system with permission denied message
    (assert-on-screen s "SYSTEM")
    (assert (wait-for-screen-contains s "Keine Berechtigung" :timeout 2))))

;;; Admin user can access admin screen

(define-test e2e-role-granted-with-admin ()
  (with-test-db (db)
    (create-test-user "adminuser" "testpass" :roles '(:veron-administrator))
    (veron::load-chat-from-db)
    (with-test-app (s veron::*veron-app*)
      (login s "adminuser" "testpass")
      (assert-on-screen s "MAIN")
      ;; Navigate to system → users
      (move-cursor s 21 14)
      (erase-eof s)
      (type-text s "5")
      (press-enter s)
      (assert-on-screen s "SYSTEM")
      (move-cursor s 21 14)
      (erase-eof s)
      (type-text s "3")
      (press-enter s)
      (assert-on-screen s "USERS"))))

;;; Direct navigation by name is also denied

(define-test e2e-role-denied-direct-navigation ()
  (with-veron-app (s :username "normaluser2" :password "testpass")
    (login s "normaluser2" "testpass")
    (assert-on-screen s "MAIN")
    ;; Try to navigate directly to users screen via command
    (move-cursor s 21 14)
    (erase-eof s)
    (type-text s "users")
    (press-enter s)
    ;; Should stay on main with permission denied
    (assert-on-screen s "MAIN")
    (assert (wait-for-screen-contains s "Keine Berechtigung" :timeout 2))))
