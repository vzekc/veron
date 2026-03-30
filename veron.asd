(asdf:defsystem #:veron
  :description "V.z.E.k.C. Electronic Retro Online Network"
  :author "Hans Hübner"
  :license "MIT"
  :serial t
  :pathname ""
  :components ((:module "src"
                :components ((:file "package")
                             (:file "dotenv")
                             (:file "db")
                             (:file "user")
                             (:file "otp")
                             (:file "files")
                             (:file "notifications")
                             (:file "notification-review-screen")
                             (:file "chat")
                             (:file "who-screen")
                             (:file "guestbook-screen")
                             (:file "chat-screen")
                             (:file "changelog-screen")
                             (:file "users-screen")
                             (:file "lu-config-screen")
                             (:file "retrostar-db")
                             (:file "network-screen")
                             (:file "decnet-screen")
                             (:file "events-screen")
                             (:file "exhibitron-db")
                             (:file "cc-screen")
                             (:file "veron"))))
  :depends-on (#:lispf #:lispf-edit #:woltlab-login #:postmodern #:dexador #:ironclad #:swank #:cl-smtp)
  :in-order-to ((asdf:test-op (asdf:test-op #:veron-test))))
