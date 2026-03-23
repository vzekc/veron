;;;; run-tests.lisp — Load and run all test suites (LISPF + VERON).
;;;;
;;;; Requires PostgreSQL with VERON_DB_* environment variables set.

(defun setup-registry (directory-path)
  (mapc (lambda (asd-pathname)
          (pushnew (make-pathname :name nil :type nil :version nil
                                  :defaults asd-pathname)
                   asdf:*central-registry*
                   :test #'equal))
        (directory (merge-pathnames #p"**/*.asd" directory-path))))

(setup-registry (make-pathname :defaults *load-truename* :name nil :type nil))

;;; Load VERON (includes LISPF as dependency)
(ql:quickload :veron)
(ql:quickload :veron-test)

;;; Load all LISPF test suites
(load (merge-pathnames "lispf/load-tests.lisp" *load-truename*))

;;; Run all registered test suites (LISPF + VERON)
(unless (lispf-test:run-all-suites)
  (uiop:quit 1))
