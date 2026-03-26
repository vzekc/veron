;;; -*- Mode: Lisp -*-

;;; OTP login and local password authentication for non-TLS connections.

(in-package #:veron)

;;; OTP generation

(defun generate-otp ()
  "Generate a 6-digit numeric one-time code."
  (format nil "~6,'0D" (random 1000000)))

;;; Email masking

(defun mask-email (email)
  "Mask an email for display: \"hans@example.com\" -> \"h***@example.com\"."
  (let ((at-pos (position #\@ email)))
    (if (and at-pos (> at-pos 0))
        (format nil "~C***~A" (char email 0) (subseq email at-pos))
        "***")))

;;; WoltLab lookup wrappers

(defun lookup-woltlab-email (username)
  "Look up a user's email from the WoltLab database."
  (when (env "VERON_AUTH_DB_HOST" nil)
    (wl:lookup-user-email username
                          :host (env "VERON_AUTH_DB_HOST")
                          :port (parse-integer (env "VERON_AUTH_DB_PORT"))
                          :database (env "VERON_AUTH_DB_NAME")
                          :user (env "VERON_AUTH_DB_USER")
                          :db-password (env "VERON_AUTH_DB_PASSWORD"))))

(defun lookup-woltlab-user (username)
  "Look up user data from WoltLab without authentication."
  (when (env "VERON_AUTH_DB_HOST" nil)
    (wl:lookup-user username
                    :host (env "VERON_AUTH_DB_HOST")
                    :port (parse-integer (env "VERON_AUTH_DB_PORT"))
                    :database (env "VERON_AUTH_DB_NAME")
                    :user (env "VERON_AUTH_DB_USER")
                    :db-password (env "VERON_AUTH_DB_PASSWORD"))))

;;; Email sending

(defun send-otp-email (email username code)
  "Send the OTP code via email. Signals application-error on failure."
  (let ((smtp-host (env "VERON_SMTP_HOST" nil)))
    (unless smtp-host
      (lspf:application-error "E-Mail-Versand nicht konfiguriert"))
    (let ((from (env "VERON_SMTP_FROM"))
          (port (parse-integer (env "VERON_SMTP_PORT" "587")))
          (smtp-user (env "VERON_SMTP_USER" nil))
          (smtp-password (env "VERON_SMTP_PASSWORD" nil)))
      (handler-case
          (cl-smtp:send-email
           smtp-host from email
           (format nil "VERON Einmalpasswort: ~A" code)
           (format nil "Hallo ~A,~%~%~
               Dein Einmalpasswort fuer VERON ist: ~A~%~%~
               Das Passwort ist 5 Minuten gueltig.~%~%~
               Da Deine Verbindung nicht verschluesselt ist, kannst Du Dich ~
               nicht mit Deinem Forum-Passwort anmelden. Nach der Anmeldung ~
               mit dem Einmalpasswort wirst Du aufgefordert, ein lokales ~
               Passwort zu setzen. Bitte verwende ein Passwort, das Du ~
               nirgendwo anders benutzt.~%"
                  username code)
           :port port
           :ssl (when (env "VERON_SMTP_TLS" nil) :starttls)
           :authentication (when smtp-user
                             (list smtp-user smtp-password)))
        (error (e)
          (lspf:log-message :error "Failed to send OTP email to ~A: ~A" email e)
          (lspf:application-error "E-Mail konnte nicht gesendet werden"))))))

;;; OTP phase management

(defun ensure-user-row (username)
  "Ensure a users row exists for USERNAME by looking up WoltLab data.
Required before store-otp, which UPDATEs the row."
  (let ((result (lookup-woltlab-user username)))
    (when result
      (with-db
        (pomo:execute
         "INSERT INTO users (id, name) VALUES ($1, $2) ON CONFLICT (id) DO NOTHING"
         (getf result :user-id) (getf result :username))))))

(defun prepare-otp-login (username)
  "Look up email, generate OTP if needed, send email. Store state on session.
Reuses an active OTP from the database if one exists.
Returns the masked email string. Signals application-error on failure."
  (let ((email (lookup-woltlab-email username)))
    (unless email
      (lspf:application-error "Benutzer nicht gefunden"))
    (when (string= email "")
      (lspf:application-error "Keine E-Mail-Adresse hinterlegt"))
    (ensure-user-row username)
    (multiple-value-bind (existing minutes-ago) (get-active-otp username)
      (let ((session lspf:*session*))
        (setf (session-otp-username session) username
              (session-otp-email-masked session) (mask-email email)
              (session-otp-attempts session) 0)
        (if existing
            (setf (lspf:session-property session :otp-minutes-ago) minutes-ago)
            (let ((code (generate-otp)))
              (setf (lspf:session-property session :otp-minutes-ago) nil)
              (store-otp username code 300)
              (send-otp-email email username code)))))
    (mask-email email)))

;;; OTP verification

(defun verify-otp (username code)
  "Verify OTP code and complete login.
Returns the screen to navigate to on success. Signals application-error on failure."
  (let ((session lspf:*session*))
    (when (string= code "")
      (lspf:application-error "Bitte Einmalpasswort eingeben"))
    (unless (string= username (session-otp-username session))
      (lspf:application-error "Benutzername stimmt nicht ueberein"))
    (let ((stored-code (get-active-otp username)))
      (unless stored-code
        (lspf:application-error "Einmalpasswort abgelaufen"))
      (incf (session-otp-attempts session))
      (when (> (session-otp-attempts session) 3)
        (clear-otp username)
        (lspf:application-error "Zu viele Fehlversuche"))
      (unless (string= code stored-code)
        (lspf:application-error
         (format nil "Ungueltiges Einmalpasswort (~D Versuche verbleibend)"
                 (- 3 (session-otp-attempts session))))))
    ;; Code valid, consume it
    (clear-otp username)
    (let ((result (lookup-woltlab-user username)))
      (unless result
        (lspf:application-error "Benutzer nicht gefunden"))
      (complete-login result)
      'set-password-otp)))

;;; Local password login

(defun login-with-local-password (username password)
  "Authenticate with local password and complete login.
Returns 'main on success. Signals application-error on failure."
  (when (or (string= username "") (string= password ""))
    (lspf:application-error "Bitte Benutzername und Passwort eingeben"))
  (let ((result (authenticate-local username password)))
    (unless result
      (lspf:application-error "Ungueltiges Passwort"))
    (complete-login result)
    (post-login-screen)))

;;; Common login completion

(defun complete-login (auth-result)
  "Finalize login after successful authentication."
  (let ((user (make-user auth-result)))
    (setf (session-user lspf:*session*) user)
    (ensure-db-user user)
    (setf (session-login-id lspf:*session*)
          (record-login user
                        :terminal-type (session-term-type lspf:*session*)
                        :tls (lspf:session-tls-p lspf:*session*)))
    (lspf:log-message :info "login user=~A tls=~A"
                      (user-username user)
                      (if (lspf:session-tls-p lspf:*session*) "yes" "no"))
    (notify :login "Anmeldung"
            (format nil "~A hat sich angemeldet" (user-username user)))
    (update-my-chat-indicator)))
