;;; -*- Mode: Lisp -*-

;;; Who's online screen handlers.

(in-package #:veron)

(defun format-session-line (session now)
  "Format a single session line for the who display."
  (let* ((user (session-user session))
         (active-app (lspf:session-active-application session))
         (username (if user (user-username user) "(login)"))
         (app-name (if active-app
                       (or (lspf:application-title active-app)
                           (lspf:application-name active-app))
                       ""))
         (screen (string (lspf:session-current-screen session)))
         (idle (format-duration (- now (lspf:session-last-activity session))))
         (connected (format-duration (- now (session-connect-time session))))
         (tls (if (lspf:session-tls-p session) "Yes" "")))
    (format nil "  ~16A ~8A ~15A ~8A ~10A ~A"
            username app-name screen idle connected tls)))

(lspf:define-dynamic-area-updater who sessions ()
  (let ((now (get-universal-time))
        (lines '()))
    (bt:with-lock-held ((lspf::application-sessions-lock lspf:*application*))
      (dolist (s (lspf::application-sessions lspf:*application*))
        (let ((line (format-session-line s now))
              (current-p (eq s lspf:*session*)))
          (push (if current-p
                    (list :content line :color cl3270:+white+ :intense t)
                    line)
                lines))))
    (nreverse lines)))
