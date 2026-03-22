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
                             (:file "files")
                             (:file "veron"))))
  :depends-on (#:lispf #:lispf-edit #:woltlab-login #:postmodern))
