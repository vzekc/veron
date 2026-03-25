;;; -*- Mode: Lisp -*-

(defpackage #:veron
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads)
                    (#:lspf #:lispf)
                    (#:wl #:woltlab-login)
                    (#:pomo #:postmodern)
                    (#:editor #:lispf-editor)
                    (#:dex #:dexador))
  (:export #:start
            #:start-from-env
            #:reload
            #:create-local-user))
