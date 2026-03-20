(defun setup-registry (directory-path)
  (format *error-output* "; adding components under ~A to asdf registry~%" directory-path)
  (mapc (lambda (asd-pathname)
          (pushnew (make-pathname :name nil
                                  :type nil
                                  :version nil
                                  :defaults asd-pathname)
                   asdf:*central-registry*
                   :test #'equal))
        (directory (merge-pathnames #p"**/*.asd" directory-path))))

(setup-registry (make-pathname :defaults *load-truename* :name nil :type nil))

(asdf:load-system "veron")
