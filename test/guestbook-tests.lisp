;;; -*- Mode: Lisp -*-

;;; E2E tests for the VERON guestbook.

(in-package #:veron-tests)

;;; Anonymous guestbook entry

(define-test e2e-guestbook-anonymous-entry ()
  (with-veron-app (s)
    ;; Navigate to guestbook from login screen via PF4
    (assert-on-screen s "LOGIN")
    (press-pf s 4)
    (assert-on-screen s "GUESTBOOK")
    ;; Press PF5 to create new entry
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK-NEW")
    ;; Fill in author and message
    (type-in-field s *guestbook-new-screen* "author" "TestBot")
    ;; Message field: first repeat at row 5, col 1 (attr at row 4+1, col 0)
    (move-cursor s 5 1)
    (erase-eof s)
    (type-text s "Hello from the test suite!")
    ;; Press PF5 to save
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK-CONFIRM")
    (assert-screen-contains s "TestBot")
    (assert-screen-contains s "Hello from the test suite!")
    ;; Confirm save
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK")
    (assert-screen-contains s "Eintrag gespeichert")
    ;; Verify entry appears in list
    (assert-screen-contains s "TestBot")
    (assert-screen-contains s "Hello from the test suite!")))

;;; Logged-in guestbook entry

(define-test e2e-guestbook-logged-in-entry ()
  (with-veron-app (s)
    (login s "testuser" "testpass")
    (navigate-to s "GUESTBOOK")
    ;; Create new entry
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK-NEW")
    ;; Author should be pre-filled and non-writable
    (assert-screen-contains s "testuser")
    ;; Type message and save
    (move-cursor s 5 1)
    (erase-eof s)
    (type-text s "Authenticated entry")
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK-CONFIRM")
    (assert-screen-contains s "testuser")
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK")
    (assert-screen-contains s "Eintrag gespeichert")
    (assert-screen-contains s "testuser")
    (assert-screen-contains s "Authenticated entry")))

;;; Cursor placement on guestbook-new for logged-in user

(define-test e2e-guestbook-new-cursor-placement ()
  (with-veron-app (s)
    (login s "testuser" "testpass")
    (navigate-to s "GUESTBOOK")
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK-NEW")
    ;; Author should be pre-filled
    (assert-screen-contains s "testuser")
    ;; Cursor should be on first message input line (row 5, col 0)
    ;; The field attr is at row 4 col 79 (wrapping), content starts at row 5 col 0
    (assert-cursor-at s 5 0
                      :description "Cursor on first message field for logged-in user")))

;;; Enter key advances cursor on guestbook-new

(define-test e2e-guestbook-enter-advances-cursor ()
  (with-veron-app (s)
    (login s "testuser" "testpass")
    (navigate-to s "GUESTBOOK")
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK-NEW")
    ;; Type on first message line, press Enter to advance
    (move-cursor s 5 1)
    (type-text s "Line 1")
    (press-enter s)
    ;; Cursor should have advanced to the next row
    (assert-cursor-at s 6 0 :description "Cursor advanced to row 6 after Enter")))

;;; View guestbook entry

(define-test e2e-guestbook-view-entry ()
  (with-veron-app (s)
    ;; Create an entry first
    (press-pf s 4)
    (assert-on-screen s "GUESTBOOK")
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK-NEW")
    (type-in-field s *guestbook-new-screen* "author" "Viewer")
    (move-cursor s 5 1)
    (type-text s "Entry to view")
    (press-pf s 5)
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK")
    ;; Select the entry: move to data row, press Enter
    ;; Cursor must be on the data row for selected-list-index to work
    (move-cursor s 3 5)
    (press-enter s)
    (assert-on-screen s "GUESTBOOK-ENTRY")
    (assert-screen-contains s "Viewer")
    (assert-screen-contains s "Entry to view")
    ;; Go back
    (press-pf s 3)
    (assert-on-screen s "GUESTBOOK")))

;;; Runner

(defun run-all ()
  (load-screen-data)
  (format t "~&;;; Running VERON E2E tests~%")
  (let ((*package* (find-package :veron-tests)))
    (run-tests)))
