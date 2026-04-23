;;; -*- Mode: Lisp -*-

;;; Tests for the VERON status bus — event envelope shape and disabled
;;; mode behavior. Does not need a live broker.

(in-package #:veron-tests)

(defun envelope-to-alist (type actor payload timestamp)
  "Call encode-envelope and parse the result back to an alist for assertions."
  (let* ((json (veron::encode-envelope (veron::type-string type)
                                       actor payload timestamp)))
    (with-input-from-string (s json)
      (let ((yason:*parse-object-as* :alist))
        (yason:parse s)))))

(defun env-get (env key)
  (cdr (assoc key env :test #'equal)))

(define-test status-envelope-session-login ()
  (let* ((env (envelope-to-alist
               :session.login
               '(:id 42 :username "klaus")
               '(:terminal_type "IBM-3278-4-E" :tls t :login_id 7)
               "2026-04-22T14:32:18.123Z")))
    (assert (equal "session.login" (env-get env "type")))
    (assert (equal "2026-04-22T14:32:18.123Z" (env-get env "timestamp")))
    (let ((actor (env-get env "actor")))
      (assert (equal 42 (env-get actor "id")))
      (assert (equal "klaus" (env-get actor "username"))))
    (let ((payload (env-get env "payload")))
      (assert (equal "IBM-3278-4-E" (env-get payload "terminal_type")))
      (assert (eq t (env-get payload "tls")))
      (assert (equal 7 (env-get payload "login_id"))))))

(define-test status-envelope-guestbook-anonymous ()
  (let ((env (envelope-to-alist
              :guestbook.created
              nil
              '(:author "Anon (Gast)" :message "Hallo Welt")
              "2026-04-22T14:32:18.000Z")))
    (assert (equal "guestbook.created" (env-get env "type")))
    (assert (assoc "actor" env :test #'equal))
    (assert (null (env-get env "actor")))
    (let ((payload (env-get env "payload")))
      (assert (equal "Anon (Gast)" (env-get payload "author"))))))

(define-test status-envelope-chat-message ()
  (let ((env (envelope-to-alist
              :chat.message
              '(:id 1 :username "klaus")
              '(:channel_id 1 :message "hi there" :db_id 99)
              "2026-04-22T15:00:00.000Z")))
    (assert (equal "chat.message" (env-get env "type")))
    (let ((payload (env-get env "payload")))
      (assert (equal 1 (env-get payload "channel_id")))
      (assert (equal "hi there" (env-get payload "message")))
      (assert (equal 99 (env-get payload "db_id"))))))

(define-test status-envelope-drops-nil-payload-keys ()
  (let* ((env (envelope-to-alist
               :session.logout
               '(:id 1 :username "u")
               '(:login_id nil)
               "2026-04-22T00:00:00.000Z"))
         (payload (env-get env "payload")))
    (assert (null (env-get payload "login_id")))))

(define-test status-subject-prefix ()
  (let ((veron::*status-subject-prefix* "veron"))
    (assert (equal "veron.session.login"
                   (veron::event-subject :session.login)))
    (assert (equal "veron.chat.message"
                   (veron::event-subject :chat.message)))))

(define-test status-subject-custom-prefix ()
  (let ((veron::*status-subject-prefix* "stage.veron"))
    (assert (equal "stage.veron.guestbook.created"
                   (veron::event-subject :guestbook.created)))))

(define-test status-publish-disabled-is-noop ()
  (let ((veron::*status-client* nil))
    (assert (null (veron:publish-status :session.login
                                        :actor '(:id 1 :username "x")
                                        :payload '(:tls t))))))
