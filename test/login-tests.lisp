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

;;; Password field cleared after failed login attempt
;;; A long wrong password followed by a shorter correct password must work.
;;; Without clearing, remnants of the long password would corrupt the short one.

(define-test e2e-login-password-cleared-after-failure ()
  (let ((correct-password "short"))
    (with-veron-app (s :username "clearuser" :password correct-password)
      (assert-on-screen s "LOGIN")
      ;; Type wrong long password and submit
      (type-in-field s *login-screen* "username" "clearuser")
      (type-in-field s *login-screen* "password" "this-is-a-very-wrong-long-password")
      (press-enter s)
      ;; Should stay on login screen with error
      (assert-on-screen s "LOGIN")
      ;; Now type correct short password and login.
      ;; The server clears the password field via screen update, so
      ;; typing the short password without erasing first must work.
      (type-in-field s *login-screen* "username" "clearuser")
      (type-in-field s *login-screen* "password" correct-password)
      (press-enter s)
      (assert-on-screen s "MAIN"))))
