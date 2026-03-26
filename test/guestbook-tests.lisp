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

;;; Switching between entries with different message lengths clears old content

(define-test e2e-guestbook-entry-switch-clears-screen ()
  (with-veron-app (s)
    ;; Create entries directly in DB: first a short one, then a long one.
    ;; ORDER BY created_at DESC means newest first, so index 0 = long, 1 = short.
    (veron::add-guestbook-entry "Short" "Hi")
    (sleep 0.1)
    (veron::add-guestbook-entry "Long"
      (format nil "~{~A~^~%~}"
              (loop for i from 1 to 10
                    collect (make-string 70 :initial-element
                                         (code-char (+ (char-code #\a) (mod i 26)))))))
    ;; Navigate to guestbook list
    (press-pf s 4)
    (assert-on-screen s "GUESTBOOK")
    ;; Select the first entry (index 0 = the long one)
    (move-cursor s 3 5)
    (press-enter s)
    (assert-on-screen s "GUESTBOOK-ENTRY")
    (assert-screen-contains s "Long")
    ;; Verify long content is present on a later message row
    (let ((row10 (string-right-trim '(#\Space) (screen-row s 10))))
      (unless (plusp (length row10))
        (error 'test-failure :description "Long entry should have content on row 10"
               :expected "(non-blank)" :actual row10)))
    ;; Switch to the short entry via PF8 (next = older = index 1)
    (press-pf s 8)
    (assert-on-screen s "GUESTBOOK-ENTRY")
    (assert-screen-contains s "Short")
    (assert-screen-contains s "Hi")
    ;; Row 10 must no longer contain remnants of the long entry
    (let ((row10 (string-right-trim '(#\Space) (screen-row s 10))))
      (when (plusp (length row10))
        (error 'test-failure :description "Row 10 should be blank after switching to short entry"
               :expected "(blank)" :actual row10)))
    ;; Row 6 (message row 1) should also be blank since "Hi" fits on row 5
    (let ((row6 (string-right-trim '(#\Space) (screen-row s 6))))
      (when (plusp (length row6))
        (error 'test-failure :description "Row 6 should be blank after switching to short entry"
               :expected "(blank)" :actual row6)))))

;;; PF7 on first guestbook entry must not cause negative SQL OFFSET

(define-test e2e-guestbook-pf7-first-entry ()
  (with-veron-app (s)
    ;; Create two entries so the guestbook list has rows
    (press-pf s 4)
    (assert-on-screen s "GUESTBOOK")
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK-NEW")
    (type-in-field s *guestbook-new-screen* "author" "First")
    (move-cursor s 5 1)
    (type-text s "First entry")
    (press-pf s 5)
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK")
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK-NEW")
    (type-in-field s *guestbook-new-screen* "author" "Second")
    (move-cursor s 5 1)
    (type-text s "Second entry")
    (press-pf s 5)
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK")
    ;; Select the first entry (row 3 in the list, index 0)
    (move-cursor s 3 5)
    (press-enter s)
    (assert-on-screen s "GUESTBOOK-ENTRY")
    ;; Press PF7 at the first entry — must not error
    (press-pf s 7)
    (assert-on-screen s "GUESTBOOK-ENTRY")))

