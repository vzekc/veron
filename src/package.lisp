;;; -*- Mode: Lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

(defpackage #:veron.nats
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export #:nats-client
           #:connect
           #:disconnect
           #:publish
           #:subscribe
           #:unsubscribe
           #:connected-p))

(defpackage #:veron
  (:use #:cl #:alexandria)
  (:local-nicknames (#:bt #:bordeaux-threads)
                    (#:pomo #:postmodern)
                    (#:nats #:veron.nats))
  (:export #:start
           #:start-from-env
           #:reload
           #:create-local-user
           #:publish-status
           #:start-status-bus
           #:stop-status-bus))
