;;; -*- Mode: Lisp -*-

(defpackage #:veron
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads)
                    (#:lspf #:lispf)
                    (#:wl #:woltlab-login))
  (:export #:start))
