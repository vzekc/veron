;;; -*- Mode: Lisp -*-

;;; DECnet address allocation screen - shows area 23 blocks from RetroStar.

(in-package #:veron)

(lispf:define-list-data-getter decnet (start end)
  (let* ((total (retrostar-decnet-block-count))
         (entries (retrostar-decnet-blocks start (- end start))))
    (values (loop for e in entries
                  for hosts = (getf e :hosts)
                  collect (list :owner (or (getf e :owner) "")
                                :range (format nil "23.~D - 23.~D"
                                               (getf e :start-node)
                                               (getf e :end-node))
                                :hosts (if (or (null hosts) (db-null-p hosts))
                                           "" hosts)))
            total)))
