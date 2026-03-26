;;; Migration 024: Populate email addresses from WoltLab for existing users.

(in-package #:veron)

(let ((updated 0))
  (with-db
    (dolist (row (pomo:query "SELECT id, name FROM users WHERE email IS NULL OR email = ''"))
      (let* ((user-id (first row))
             (username (second row))
             (email (ignore-errors (lookup-woltlab-email username))))
        (when (and email (plusp (length email)))
          (pomo:execute "UPDATE users SET email = $1 WHERE id = $2" email user-id)
          (incf updated)))))
  (when (plusp updated)
    (lispf:log-message :info "populated ~D email address~:P from WoltLab" updated)))
