;;; -*- Mode: Lisp -*-

;;; Changelog screen - deployment log and news viewer.

(in-package #:veron)

(defconstant +changelog-display-lines+ 18
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

(lspf:define-screen-update changelog (page-info)
  (let* ((user (session-user lspf:*session*))
         (all-lines (changelog-lines))
         (total (length all-lines))
         (offset (or (lspf:session-property lspf:*session* :changelog-offset) 0))
         (page-num (1+ (floor offset +changelog-display-lines+)))
         (total-pages (max 1 (ceiling total +changelog-display-lines+))))
    (when user
      (mark-changelog-read (user-id user)))
    (setf page-info (format nil "Seite ~D von ~D" page-num total-pages))
    (when (> offset 0)
      (lspf:show-key :pf7 "Vor."))
    (when (< (+ offset +changelog-display-lines+) total)
      (lspf:show-key :pf8 "Naech."))
    (when (and user (admin-p user))
      (lspf:show-key :pf5 "Bearbeiten"))))

;;; Dynamic area updater

(lspf:define-dynamic-area-updater changelog content ()
  (let* ((all-lines (changelog-lines))
         (total (length all-lines))
         (offset (or (lspf:session-property lspf:*session* :changelog-offset) 0)))
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

(lspf:define-key-handler changelog :pf7 ()
  (let* ((offset (or (lspf:session-property lspf:*session* :changelog-offset) 0))
         (new-offset (max 0 (- offset +changelog-display-lines+))))
    (setf (lspf:session-property lspf:*session* :changelog-offset) new-offset))
  :stay)

(lspf:define-key-handler changelog :pf8 ()
  (let* ((all-lines (changelog-lines))
         (total (length all-lines))
         (offset (or (lspf:session-property lspf:*session* :changelog-offset) 0))
         (new-offset (+ offset +changelog-display-lines+)))
    (when (< new-offset total)
      (setf (lspf:session-property lspf:*session* :changelog-offset) new-offset)))
  :stay)

(lspf:define-key-handler changelog :pf3 ()
  (setf (lspf:session-property lspf:*session* :changelog-offset) 0)
  :back)

(lspf:define-key-handler changelog :pf5 ()
  (let ((user (session-user lspf:*session*)))
    (unless (admin-p user)
      (lspf:application-error "Keine Berechtigung"))
    (let* ((file-id (ensure-changelog-file))
           (path (file-to-disk file-id)))
      (editor:edit-file path :display-name "Changelog")
      (disk-to-file file-id)
      (cleanup-tmp-file file-id)
      (setf (lspf:session-property lspf:*session* :changelog-offset) 0)))
  :stay)
