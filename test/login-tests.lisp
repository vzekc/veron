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
