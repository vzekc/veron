;;;; load-tests.lisp — Load test suites without running them.
;;;;
;;;; After loading, run individual tests with:
;;;;   (lispf-test:run-tests 'test-name :package :package-name)
;;;; Then call (veron-tests::drop-template-db) to clean up.

(defun setup-registry (directory-path)
  (mapc (lambda (asd-pathname)
          (pushnew (make-pathname :name nil :type nil :version nil
                                  :defaults asd-pathname)
                   asdf:*central-registry*
                   :test #'equal))
        (directory (merge-pathnames #p"**/*.asd" directory-path))))

(setup-registry (make-pathname :defaults *load-truename* :name nil :type nil))

(ql:quickload :veron-test)

;; Clean up template DB on exit
(push (lambda () (ignore-errors (veron-tests::drop-template-db)))
      sb-ext:*exit-hooks*)
