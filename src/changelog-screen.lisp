;;; -*- Mode: Lisp -*-

;;; Changelog screen - deployment log and news viewer.

(in-package #:veron)

(defconstant +changelog-display-lines+ 20
  "Number of visible content lines in the changelog dynamic area.")

;;; Changelog text splitting

(defun changelog-lines ()
  "Return the changelog content split into display lines, wrapping at 80 columns."
  (let ((text (load-changelog-text))
        (lines '()))
    (dolist (raw-line (uiop:split-string text :separator '(#\Newline)))
      (if (<= (length raw-line) 80)
          (push raw-line lines)
          (loop for start from 0 by 80
                while (< start (length raw-line))
                do (push (subseq raw-line start
                                 (min (+ start 80) (length raw-line)))
                         lines))))
    (nreverse lines)))

;;; Screen update

(lispf:define-screen-update changelog (page-info)
  (let* ((user (session-user lispf:*session*))
         (post-login (lispf:session-property lispf:*session* :changelog-post-login))
         (all-lines (changelog-lines))
         (total (length all-lines))
         (offset (or (lispf:session-property lispf:*session* :changelog-offset) 0))
         (page-num (1+ (floor offset +changelog-display-lines+)))
         (total-pages (max 1 (ceiling total +changelog-display-lines+))))
    (when user
      (mark-changelog-read (user-id user)))
    (setf page-info (format nil "Seite ~D von ~D" page-num total-pages))
    (if post-login
        (progn
          (lispf:hide-key :pf3)
          (lispf:show-key :enter "Weiter"))
        (lispf:show-key :pf3 "Zurueck"))
    (when (> offset 0)
      (lispf:show-key :pf7 "Vor."))
    (when (< (+ offset +changelog-display-lines+) total)
      (lispf:show-key :pf8 "Naech."))
    (when (and user (admin-p user))
      (lispf:show-key :pf4 "Bearbeiten")
      (lispf:show-key :pf5 "Neu"))))

;;; Dynamic area updater

(lispf:define-dynamic-area-updater changelog content ()
  (let* ((all-lines (changelog-lines))
         (total (length all-lines))
         (offset (or (lispf:session-property lispf:*session* :changelog-offset) 0)))
    (loop for i below +changelog-display-lines+
          for line-idx = (+ offset i)
          collect (if (< line-idx total)
                      (let ((line (nth line-idx all-lines)))
                        (if (and (>= (length line) 3)
                                 (string= (subseq line 0 3) "==="))
                            (list :content line :color cl3270:+yellow+ :intense t)
                            line))
                      ""))))

;;; Key handlers

(lispf:define-key-handler changelog :pf7 ()
  (let* ((offset (or (lispf:session-property lispf:*session* :changelog-offset) 0))
         (new-offset (max 0 (- offset +changelog-display-lines+))))
    (setf (lispf:session-property lispf:*session* :changelog-offset) new-offset))
  :stay)

(lispf:define-key-handler changelog :pf8 ()
  (let* ((all-lines (changelog-lines))
         (total (length all-lines))
         (offset (or (lispf:session-property lispf:*session* :changelog-offset) 0))
         (new-offset (+ offset +changelog-display-lines+)))
    (when (< new-offset total)
      (setf (lispf:session-property lispf:*session* :changelog-offset) new-offset)))
  :stay)

(lispf:define-key-handler changelog :enter ()
  (if (lispf:session-property lispf:*session* :changelog-post-login)
      (progn
        (setf (lispf:session-property lispf:*session* :changelog-offset) 0
              (lispf:session-property lispf:*session* :changelog-post-login) nil)
        'main)
      :stay))

(lispf:define-key-handler changelog :pf3 ()
  (if (lispf:session-property lispf:*session* :changelog-post-login)
      :stay
      (progn
        (setf (lispf:session-property lispf:*session* :changelog-offset) 0)
        :back)))

(defun format-manual-changelog-heading (username)
  "Format a changelog heading for a manual entry by USERNAME with three empty lines."
  (with-output-to-string (s)
    (multiple-value-bind (sec min hour day month year)
        (decode-display-time (get-universal-time))
      (declare (ignore sec))
      (format s "=== Eintrag von ~A, ~2,'0D.~2,'0D.~4D ~2,'0D:~2,'0D ===~%"
              username day month year hour min))
    (dotimes (i 3) (terpri s))))

(defun edit-changelog (&optional prepend)
  "Open the changelog in the editor. If PREPEND is non-nil, prepend it first."
  (unless (admin-p (session-user lispf:*session*))
    (lispf:application-error "Keine Berechtigung"))
  (let ((file-id (ensure-changelog-file)))
    (when prepend
      (save-changelog-text (concatenate 'string prepend (load-changelog-text))))
    (let ((path (file-to-disk file-id)))
      (lispf-editor:edit-file path :display-name "Changelog")
      (disk-to-file file-id)
      (cleanup-tmp-file file-id))
    (setf (lispf:session-property lispf:*session* :changelog-offset) 0))
  :stay)

(lispf:define-key-handler changelog :pf4 ()
  (edit-changelog))

(lispf:define-key-handler changelog :pf5 ()
  (edit-changelog (format-manual-changelog-heading
                   (user-username (session-user lispf:*session*)))))
