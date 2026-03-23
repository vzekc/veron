(asdf:defsystem #:veron-test
  :description "Tests for VERON"
  :serial t
  :pathname ""
  :components ((:module "test"
                :components ((:file "packages")
                             (:file "harness")
                             (:file "guestbook-tests"))))
  :depends-on (#:veron #:lispf-test))
