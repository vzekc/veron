;;; -*- Mode: Lisp -*-

;;; Who's online screen handlers.

(in-package #:veron)

(defun format-session-line (session now)
  "Format a single session line for the who display."
  (let* ((user (session-user session))
         (active-app (lispf:session-active-application session))
         (username (if user (user-username user) "(login)"))
         (app-name (if active-app
                       (or (lispf:application-title active-app)
                           (lispf:application-name active-app))
                       ""))
         (screen (string (lispf:session-current-screen session)))
         (lu (let ((name (session-lu-name session)))
               (if (or (null name) (string= name "*")) "" name)))
         (idle-secs (- now (lispf:session-last-activity session)))
         (idle (if (< idle-secs 60) "" (format-duration idle-secs)))
         (connected (format-duration (- now (session-connect-time session))))
         (tls (if (lispf:session-tls-p session) "Yes" "")))
    (format nil "  ~16A ~8A ~15A ~8A ~6A ~6A ~A"
            username app-name screen lu idle connected tls)))

(lispf:define-dynamic-area-updater who sessions ()
  (let ((now (get-universal-time))
        (lines '()))
    (bt:with-lock-held ((lispf::application-sessions-lock lispf:*application*))
      (dolist (s (lispf::application-sessions lispf:*application*))
        (let ((line (format-session-line s now))
              (current-p (eq s lispf:*session*)))
          (push (if current-p
                    (list :content line :color cl3270:+white+ :intense t)
                    line)
                lines))))
    (nreverse lines)))
