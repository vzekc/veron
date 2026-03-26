;;; -*- Mode: Lisp -*-

;;; E2E tests for login with special characters in passwords.

(in-package #:veron-tests)

;;; Password with # character

(define-test e2e-login-password-with-hash ()
  (with-veron-app (s :username "hashuser" :password "abc#def")
    (login s "hashuser" "abc#def")))

;;; Password with all safe special characters

(define-test e2e-login-password-with-specials ()
  (with-veron-app (s :username "specuser" :password "a!b@c#d$e%f")
    (login s "specuser" "a!b@c#d$e%f")))

;;; Long password (40 characters)

(define-test e2e-login-long-password ()
  (let ((password "abcdefghijklmnopqrstuvwxyz01234567890ABC"))
    (with-veron-app (s :username "longpwuser" :password password)
      (login s "longpwuser" password))))

;;; Password with spaces

(define-test e2e-login-password-with-spaces ()
  (with-veron-app (s :username "spaceuser" :password "pass word 123")
    (login s "spaceuser" "pass word 123")))

;;; Enter on username field moves cursor to password field start (TLS mode)

(define-test e2e-login-enter-cursor-to-password ()
  (setf (lispf:application-test-force-tls veron::*veron-app*) t)
  (unwind-protect
       (with-veron-app (s :username "testuser" :password "testpass")
         (assert-on-screen s "LOGIN")
         ;; Type username and press Enter — should move cursor to password field
         (type-text s "testuser")
         (press-enter s)
         (assert-on-screen s "LOGIN")
         ;; Password field :from (19 19), +1 row for title = row 20
         ;; Data starts at col 19 (attribute byte at col 18)
         (assert-cursor-at s 20 19 :description "Cursor at start of password field"))
    (setf (lispf:application-test-force-tls veron::*veron-app*) nil)))

;;; Password field cleared after failed login attempt
;;; A long wrong password followed by a shorter correct password must work.
;;; Without clearing, remnants of the long password would corrupt the short one.

(define-test e2e-login-password-cleared-after-failure ()
  (let ((correct-password "short"))
    (with-veron-app (s :username "clearuser" :password correct-password)
      (assert-on-screen s "LOGIN")
      ;; Enter username to get to login-local
      (type-text s "clearuser")
      (press-enter s)
      (assert-on-screen s "LOGIN-LOCAL")
      ;; Type wrong long password and submit
      (type-text s "this-is-a-very-wrong-long-password")
      (press-enter s)
      ;; Should stay on login-local with error
      (assert-on-screen s "LOGIN-LOCAL")
      ;; Now type correct short password and login
      (type-text s correct-password)
      (press-enter s)
      (assert-on-screen s "MAIN"))))
