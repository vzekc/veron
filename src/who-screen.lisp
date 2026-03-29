;;; -*- Mode: Lisp -*-

;;; Who's online screen handlers.

(in-package #:veron)

(defun format-connection-line (conn now)
  "Format a single connection line for the who display."
  (let* ((session (lispf:connection-session conn))
         (user (when (typep session 'authenticated-session)
                 (session-user session)))
         (active-app (when session (lispf:session-active-application session)))
         (username (if user (user-username user) "(login)"))
         (app-name (if active-app
                       (or (lispf:application-title active-app)
                           (lispf:application-name active-app))
                       ""))
         (screen (if session (string (lispf:session-current-screen session)) ""))
         (lu (let ((name (connection-lu-name conn)))
               (if (or (null name) (string= name "*")) "" name)))
         (idle-secs (- now (lispf:connection-last-activity conn)))
         (idle (if (< idle-secs 60) "" (format-duration idle-secs)))
         (connected (format-duration (- now (connection-connect-time conn))))
         (tls (if (lispf:connection-tls-p conn) "Yes" "")))
    (format nil "  ~16A ~8A ~15A ~8A ~6A ~6A ~A"
            username app-name screen lu idle connected tls)))

(lispf:define-dynamic-area-updater who sessions ()
  (let ((now (get-universal-time))
        (lines '()))
    (bt:with-lock-held ((lispf:application-connections-lock lispf:*application*))
      (dolist (conn (lispf:application-connections lispf:*application*))
        (let ((line (format-connection-line conn now))
              (current-p (eq (lispf:connection-session conn) lispf:*session*)))
          (push (if current-p
                    (list :content line :color cl3270:+white+ :intense t)
                    line)
                lines))))
    (nreverse lines)))
