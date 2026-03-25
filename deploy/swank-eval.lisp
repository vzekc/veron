;;;; -*- Mode: Lisp -*-

;;;; swank-eval.lisp
;;;;
;;;; Minimal Swank wire protocol client for evaluating expressions
;;;; in a running Swank server.  Based on the approach used by lisp-mcp:
;;;; character streams with string-based wire protocol framing.
;;;;
;;;; Usage: SWANK_PORT=4005 SWANK_EVAL='(veron:reload)' \
;;;;          sbcl --noinform --non-interactive \
;;;;            --no-userinit --no-sysinit \
;;;;            --load deploy/swank-eval.lisp
;;;;
;;;; Environment:
;;;;   SWANK_PORT (required) - port of the running Swank server
;;;;   SWANK_HOST (default "127.0.0.1") - host of the Swank server
;;;;   SWANK_EVAL (required) - Lisp expression to evaluate

(require :sb-bsd-sockets)

(defpackage #:swank-eval
  (:use #:cl))

(in-package #:swank-eval)

(defun swank-send (stream message)
  "Send MESSAGE string with Swank wire protocol length prefix."
  (write-string (format nil "~6,'0X" (length message)) stream)
  (write-string message stream)
  (force-output stream))

(defun swank-recv (stream)
  "Read one Swank wire protocol message.  Returns the raw string."
  (let ((header (make-string 6)))
    (read-sequence header stream)
    (let* ((len (parse-integer header :radix 16))
           (payload (make-string len)))
      (read-sequence payload stream)
      payload)))

(defun slime-secret ()
  "Read the Swank secret from ~/.slime-secret if it exists."
  (let ((path (merge-pathnames ".slime-secret" (user-homedir-pathname))))
    (when (probe-file path)
      (with-open-file (s path)
        (string-trim '(#\Space #\Newline #\Return #\Tab)
                     (read-line s nil ""))))))

(defun msg-starts-with (msg prefix)
  (and (>= (length msg) (length prefix))
       (string= prefix (subseq msg 0 (length prefix)))))

(defun extract-numbers-after (msg prefix)
  "Extract two integers after PREFIX in MSG.  E.g. from '(:debug 123 1 ...'
extract 123 and 1."
  (let ((start (length prefix)))
    (multiple-value-bind (n1 pos1) (parse-integer msg :start start :junk-allowed t)
      (when n1
        (multiple-value-bind (n2 pos2) (parse-integer msg :start pos1 :junk-allowed t)
          (declare (ignore pos2))
          (when n2
            (values n1 n2)))))))

(defun wait-for-return (stream)
  "Read Swank messages until a :return message arrives.
Prints :write-string output to stdout.  On :debug (error), extracts the
error message, aborts the debugger, and continues waiting for :return."
  (let ((error-message nil))
    (loop
      (let ((msg (swank-recv stream)))
        (cond
          ((msg-starts-with msg "(:write-string")
           (let* ((start (position #\" msg :start 14))
                  (end (when start (position #\" msg :start (1+ start)))))
             (when (and start end)
               (write-string (subseq msg (1+ start) end)))))
          ((msg-starts-with msg "(:debug ")
           ;; Extract error description (first quoted string)
           (let* ((start (position #\" msg))
                  (end (when start (position #\" msg :start (1+ start)))))
             (when (and start end)
               (setf error-message (subseq msg (1+ start) end)))))
          ((msg-starts-with msg "(:debug-activate")
           ;; Abort the debugger: invoke restart 1 (*ABORT)
           (multiple-value-bind (thread level)
               (extract-numbers-after msg "(:debug-activate")
             (when (and thread level)
               (swank-send stream
                           (format nil "(:emacs-rex (swank:invoke-nth-restart-for-emacs ~D 1) ~S ~D 2)"
                                   level "COMMON-LISP-USER" thread)))))
          ((msg-starts-with msg "(:return")
           (return (if error-message
                       (format nil "(:return (:abort ~S))" error-message)
                       msg))))))))

(defun connect (host port)
  "Open a character TCP connection to HOST:PORT."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp))
        (addr (sb-bsd-sockets:make-inet-address host)))
    (sb-bsd-sockets:socket-connect socket addr port)
    (sb-bsd-sockets:socket-make-stream socket
                                       :input t :output t
                                       :element-type 'character
                                       :buffering :full)))

(defun main ()
  (let ((port-string (sb-ext:posix-getenv "SWANK_PORT"))
        (host (or (sb-ext:posix-getenv "SWANK_HOST") "127.0.0.1"))
        (expression (sb-ext:posix-getenv "SWANK_EVAL")))
    (unless port-string
      (format *error-output* "SWANK_PORT not set~%")
      (sb-ext:exit :code 1))
    (unless expression
      (format *error-output* "SWANK_EVAL not set~%")
      (sb-ext:exit :code 1))
    (handler-case
        (let* ((port (parse-integer port-string))
               (stream (connect host port)))
          (unwind-protect
               (progn
                 (let ((secret (slime-secret)))
                   (when secret
                     (swank-send stream secret)))
                 (swank-send stream
                             (format nil "(:emacs-rex (swank:eval-and-grab-output ~S) ~S t 1)"
                                     expression "COMMON-LISP-USER"))
                 (let ((result (wait-for-return stream)))
                   (cond
                     ((search ":ok" result)
                      (format t "~&OK~%")
                      (sb-ext:exit :code 0))
                     (t
                      (format *error-output* "~&Eval failed: ~A~%" result)
                      (sb-ext:exit :code 1)))))
            (ignore-errors (close stream))))
      (error (e)
        (format *error-output* "~&Error: ~A~%" e)
        (sb-ext:exit :code 1)))))

(main)
