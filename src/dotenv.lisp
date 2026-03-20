;;; -*- Mode: Lisp -*-

(in-package #:veron)

(defun load-dotenv ()
  "Load variables from .env file in the system source directory, if it exists.
Does not override variables already set in the environment."
  (let ((path (merge-pathnames #P".env" (asdf:system-source-directory :veron))))
    (when (probe-file path)
      (with-open-file (in path)
        (loop for line = (read-line in nil)
              while line
              do (let ((trimmed (string-trim '(#\Space #\Tab) line)))
                   (when (and (plusp (length trimmed))
                              (char/= (char trimmed 0) #\#))
                     (let ((pos (position #\= trimmed)))
                       (when pos
                         (let ((key (subseq trimmed 0 pos))
                               (val (subseq trimmed (1+ pos))))
                           (unless (uiop:getenv key)
                             (setf (uiop:getenv key) val))))))))))))

(load-dotenv)

(defun env (name)
  "Get an environment variable, signaling an error if not set."
  (or (uiop:getenv name)
      (error "Environment variable ~A is not set" name)))
