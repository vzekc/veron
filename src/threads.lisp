;;; -*- Mode: Lisp -*-

;;; Ending a thread by taking its work away and waiting, rather than killing it.

(in-package #:veron)

(defparameter *thread-end-seconds* 5
  "How long a thread is given to notice that its socket has been closed.")

(defun wait-for-thread-end (thread what &key (seconds *thread-end-seconds*))
  "Wait for THREAD to run out, and return T once it has.
WHAT names the thread in the log when it stays.

A thread killed where it stands dies wherever it happens to be, and where it
happens to be may be inside a lock the C library holds: every log line takes
one through localtime, and one taken by a thread that no longer exists brings
the whole process down as soon as another thread wants it. So the work is
taken away -- a socket closed, a flag cleared -- and the thread is left to
notice; one that stays is reported and left running."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop while (and (bt:thread-alive-p thread)
                     (< (get-internal-real-time) deadline))
          do (sleep 0.01))
    (or (not (bt:thread-alive-p thread))
        (progn
          (lispf:log-message :warn "~A is still running after ~D seconds" what seconds)
          nil))))
