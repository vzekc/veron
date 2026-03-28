(asdf:defsystem #:veron-test
  :description "Tests for VERON (includes all LISPF tests)"
  :serial t
  :pathname ""
  :components ((:module "test"
                :components ((:file "packages")
                             (:file "harness")
                             (:file "login-tests")
                             (:file "guestbook-tests")
                             (:file "chat-tests")
                             (:file "logout-tests")
                             (:file "role-tests")
                             (:file "confirmation-tests")
                             (:file "menu-tests")
                             (:file "cc-tests")
                             (:file "retrostar-tests")
                             (:file "lu-config-tests"))))
  :depends-on (#:veron #:lispf/tests)
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :lispf-test :run-all-suites)))
