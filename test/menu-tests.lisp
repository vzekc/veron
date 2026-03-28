;;; -*- Mode: Lisp -*-

;;; E2E tests for menu navigation.

(in-package #:veron-tests)

;;; Numeric option on submenu should not leak to main menu

(define-test e2e-menu-no-leak-to-main ()
  (with-veron-app (s :username "menuuser" :password "menupass")
    (login s "menuuser" "menupass")
    (assert-on-screen s "MAIN")
    ;; Navigate to system submenu
    (select-menu-item s "System")
    (assert-on-screen s "SYSTEM")
    ;; Type "99" — no such item, should show error
    (move-cursor s 21 14)
    (erase-eof s)
    (type-text s "99")
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
    (select-menu-item s "System")
    (assert-on-screen s "SYSTEM")
    ;; Select "Protokoll" — should go to log screen
    (select-menu-item s "Protokoll")
    (assert-on-screen s "LOG")))

;;; PF1 on menu screen should open help, not show unknown key

(define-test e2e-menu-pf1-opens-help ()
  (with-veron-app (s :username "menuhelpuser" :password "menuhelppass")
    (login s "menuhelpuser" "menuhelppass")
    (assert-on-screen s "MAIN")
    (press-key s :pf1)
    (assert-on-screen s "HELP-VIEWER")))

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
