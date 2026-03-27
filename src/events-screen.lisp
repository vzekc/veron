;;; -*- Mode: Lisp -*-

;;; Network event log screen - shows events from RetroStar.

(in-package #:veron)

(lispf:define-list-data-getter events (start end)
  (let* ((total (retrostar-event-count))
         (entries (retrostar-events start (- end start))))
    (values (loop for e in entries
                  for ts = (getf e :timestamp)
                  collect (list :timestamp (if (or (null ts) (db-null-p ts))
                                               "" (format-datetime ts))
                                :event-type (or (getf e :type) "")
                                :message (or (getf e :message) "")))
            total)))
