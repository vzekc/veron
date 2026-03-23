;;;; run-tests.lisp — Load all test suites and run them.
;;;;
;;;; Runs both LISPF framework tests and VERON application tests.
;;;; Requires PostgreSQL with VERON_DB_* environment variables set.

(defun setup-registry (directory-path)
  (mapc (lambda (asd-pathname)
          (pushnew (make-pathname :name nil :type nil :version nil
                                  :defaults asd-pathname)
                   asdf:*central-registry*
                   :test #'equal))
        (directory (merge-pathnames #p"**/*.asd" directory-path))))

(setup-registry (make-pathname :defaults *load-truename* :name nil :type nil))

;;; Load systems
(ql:quickload :veron)
(ql:quickload :veron-test)
(ql:quickload :lispf-test)
(ql:quickload :lispf-edit)
(ql:quickload :lispf-guestbook)

;;; Load LISPF test suites
(let ((*default-pathname-defaults* (asdf:system-source-directory :lispf)))
  (load "test/i18n-tests.lisp")
  (load "test/cursor-tests.lisp")
  (load "test/help-tests.lisp")
  (load "editor/test/editor-tests.lisp")
  (load "examples/guestbook/guestbook-tests.lisp"))

;;; Run all registered test suites (LISPF + VERON)
(unless (lispf-test:run-all-suites)
  (uiop:quit 1))
