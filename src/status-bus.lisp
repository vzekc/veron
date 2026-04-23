;;; -*- Mode: Lisp -*-

;;; Status bus — publishes VERON status events to NATS.
;;;
;;; Event sites in the application call PUBLISH-STATUS with an event type
;;; keyword, an optional actor (plist with :id/:username), and a payload
;;; plist. The bus builds the JSON envelope and hands it to the NATS client.
;;; When VERON_NATS_URL is unset, the bus is a no-op — nothing starts and
;;; PUBLISH-STATUS returns immediately.

(in-package #:veron)

(defvar *status-client* nil
  "The live NATS client used by the status bus, or NIL when disabled.")

(defvar *status-subject-prefix* "veron"
  "Prefix prepended to every status subject, set from VERON_NATS_SUBJECT_PREFIX.")

(defun status-bus-enabled-p ()
  (not (null *status-client*)))

(defun iso8601-now ()
  "UTC ISO 8601 timestamp with millisecond precision."
  (multiple-value-bind (sec min hour day month year dow dst-p tz)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore dow dst-p tz))
    (let* ((itu internal-time-units-per-second)
           (fraction (mod (get-internal-real-time) itu))
           (ms (floor (* 1000 fraction) itu)))
      (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D.~3,'0DZ"
              year month day hour min sec ms))))

(defun plist-to-ht (plist)
  "Convert a plist with keyword keys to a hash table with string keys,
skipping keys whose value is NIL."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on plist by #'cddr
          when v
          do (setf (gethash (string-downcase (symbol-name k)) ht) v))
    ht))

(defun build-envelope (type actor payload timestamp)
  "Build the JSON envelope hash table for a status event."
  (let ((env (make-hash-table :test 'equal)))
    (setf (gethash "type" env) type
          (gethash "timestamp" env) timestamp
          (gethash "actor" env) (if actor
                                    (plist-to-ht actor)
                                    :null)
          (gethash "payload" env) (if payload
                                      (plist-to-ht payload)
                                      (make-hash-table :test 'equal)))
    env))

(defun encode-envelope (type actor payload timestamp)
  (let ((yason:*symbol-encoder* 'yason:encode-symbol-as-lowercase))
    (with-output-to-string (s)
      (yason:encode (build-envelope type actor payload timestamp) s))))

(defun event-subject (type-keyword)
  (format nil "~A.~A" *status-subject-prefix*
          (string-downcase (symbol-name type-keyword))))

(defun type-string (type-keyword)
  (string-downcase (symbol-name type-keyword)))

(defun publish-status (type &key actor payload)
  "Publish a status event. TYPE is a keyword like :session.login — the dot
is carried through to the NATS subject. ACTOR is a plist (:id … :username …)
or NIL for anonymous events. PAYLOAD is a plist; NIL-valued keys are dropped.

Never blocks; returns no values when the bus is disabled."
  (unless *status-client*
    (return-from publish-status))
  (handler-case
      (let* ((ts (iso8601-now))
             (body (encode-envelope (type-string type) actor payload ts))
             (subject (event-subject type)))
        (nats:publish *status-client* subject body))
    (error (e)
      (lispf:log-message :error "status bus publish failed (~A): ~A" type e))))

(defun start-status-bus ()
  "Start the NATS client if VERON_NATS_URL is set.
Idempotent — calling twice is a no-op."
  (when *status-client*
    (return-from start-status-bus))
  (let ((url (uiop:getenv "VERON_NATS_URL"))
        (prefix (uiop:getenv "VERON_NATS_SUBJECT_PREFIX")))
    (when (and prefix (plusp (length prefix)))
      (setf *status-subject-prefix* prefix))
    (cond
      ((or (null url) (zerop (length url)))
       (lispf:log-message :info "status bus disabled (VERON_NATS_URL unset)"))
      (t
       (setf veron.nats::*log-function*
             (lambda (msg) (lispf:log-message :info "~A" msg)))
       (setf *status-client* (nats:connect url :name "veron"))
       (lispf:log-message :info "status bus connecting to ~A" url)))))

(defun stop-status-bus ()
  "Disconnect the NATS client. Safe to call when the bus is already stopped."
  (when *status-client*
    (let ((c *status-client*))
      (setf *status-client* nil)
      (ignore-errors (nats:disconnect c)))))
