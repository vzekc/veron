;;; -*- Mode: Lisp -*-

(in-package #:veron)

(defun load-dotenv-file (path)
  "Load variables from a .env file at PATH, if it exists.
Does not override variables already set in the environment."
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
                           (setf (uiop:getenv key) val)))))))))))

(defun load-dotenv ()
  "Load variables from .env files. Checks the current directory first,
then the system source directory. Does not override existing variables."
  (load-dotenv-file (merge-pathnames #P".env" (uiop:getcwd)))
  (load-dotenv-file (merge-pathnames #P".env" (asdf:system-source-directory :veron))))

(load-dotenv)

(defun env (name &optional (default nil default-provided-p))
  "Get an environment variable, signaling an error if not set."
  (or (uiop:getenv name)
      (if default-provided-p
          default
          (error "Environment variable ~A is not set" name))))
