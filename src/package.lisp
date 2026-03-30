;;; -*- Mode: Lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

(defpackage #:veron
  (:use #:cl #:alexandria)
  (:local-nicknames (#:bt #:bordeaux-threads)
                    (#:pomo #:postmodern))
  (:export #:start
           #:start-from-env
           #:reload
           #:create-local-user))
