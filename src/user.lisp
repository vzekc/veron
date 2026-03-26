;;; -*- Mode: Lisp -*-

(in-package #:veron)

(defclass user ()
  ((id :initarg :id :reader user-id)
   (username :initarg :username :reader user-username)
   (email :initarg :email :reader user-email)
   (groups :initarg :groups :reader user-groups)
   (login-time :initarg :login-time :reader user-login-time)))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type t)
    (format stream "~A (ID ~D)" (user-username user) (user-id user))))

;;; Roles

(defvar *available-roles* '(:veron-administrator)
  "List of all defined roles in the system.")

(defvar *admin-groups* '("Administratoren")
  "List of WoltLab group names that grant admin privileges.")

(defun effective-roles (user)
  "Return the combined roles for USER: DB roles + WoltLab-derived roles."
  (let ((db-roles (mapcar (lambda (r) (intern (string-upcase r) :keyword))
                          (user-db-roles (user-id user))))
        (extra '()))
    (when (some (lambda (group)
                  (member (getf group :group-name) *admin-groups* :test #'string=))
                (user-groups user))
      (push :veron-administrator extra))
    (union db-roles extra)))

(defun admin-p (user)
  "Return T if USER has the veron-administrator role."
  (and user
       (member :veron-administrator (effective-roles user))))

(defun make-user (auth-result)
  "Create a user from an authenticate-user result plist."
  (make-instance 'user
                 :id (getf auth-result :user-id)
                 :username (getf auth-result :username)
                 :email (getf auth-result :email)
                 :groups (getf auth-result :groups)
                 :login-time (get-universal-time)))
