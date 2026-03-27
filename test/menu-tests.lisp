;;; -*- Mode: Lisp -*-

;;; E2E tests for menu navigation.

(in-package #:veron-tests)

;;; Numeric option on submenu should not leak to main menu

(define-test e2e-menu-no-leak-to-main ()
  (with-veron-app (s :username "menuuser" :password "menupass")
    (login s "menuuser" "menupass")
    (assert-on-screen s "MAIN")
    ;; Navigate to system submenu
    (move-cursor s 21 14)
    (erase-eof s)
    (type-text s "5")
    (press-enter s)
    (assert-on-screen s "SYSTEM")
    ;; Type "4" — system only has items 1-3, should show error
    (move-cursor s 21 14)
    (erase-eof s)
    (type-text s "4")
    (press-enter s)
    (assert-on-screen s "SYSTEM")
    (assert (wait-for-screen-contains s "Ungueltige Auswahl" :timeout 2)
            () "Should show invalid selection for invalid menu option")))

;;; Numeric option on submenu selects correct item

(define-test e2e-menu-submenu-selection ()
  (with-veron-app (s :username "menuuser2" :password "menupass2")
    (login s "menuuser2" "menupass2")
    (assert-on-screen s "MAIN")
    ;; Navigate to system submenu
    (move-cursor s 21 14)
    (erase-eof s)
    (type-text s "5")
    (press-enter s)
    (assert-on-screen s "SYSTEM")
    ;; Type "2" — should go to log screen
    (move-cursor s 21 14)
    (erase-eof s)
    (type-text s "2")
    (press-enter s)
    (assert-on-screen s "LOG")))

;;; Named navigation works from any screen

(define-test e2e-menu-named-navigation ()
  (with-veron-app (s :username "menuuser3" :password "menupass3")
    (login s "menuuser3" "menupass3")
    (assert-on-screen s "MAIN")
    ;; Type "system" in command field
    (navigate-to s "SYSTEM")
    ;; PF3 back to main
    (press-key s :pf3)
    (assert-on-screen s "MAIN")))
