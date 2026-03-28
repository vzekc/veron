;;; -*- Mode: Lisp -*-

;;; Tests for RetroStar screens.

(in-package #:veron-tests)

(defun events-screen-path ()
  "Find the events screen file, trying the correct name first, then the old name."
  (let ((dir (merge-pathnames #P"screens/" (asdf:system-source-directory :veron))))
    (or (first (directory (merge-pathnames "*ereignisse*.screen" dir)))
        (probe-file (merge-pathnames "events.screen" dir)))))

(defun events-screen-data ()
  "Load the events screen data."
  (let ((path (events-screen-path)))
    (when path
      (lispf-test:load-test-screen-data path))))

(defun screen-repeat-count (screen-data)
  "Return the maximum :repeat value from the screen's field definitions."
  (let ((fields (getf screen-data :fields)))
    (reduce #'max (mapcar (lambda (f) (or (getf f :repeat) 1)) fields))))

(define-test events-screen-has-specific-name ()
  "The events screen should not be named 'events' (too generic)."
  (let ((data (events-screen-data)))
    (assert data () "Could not find an *ereignisse* screen file")
    (assert (not (string-equal "events" (getf data :name))) ()
            "Screen should not be named 'events', got ~S" (getf data :name))))

(define-test events-screen-uses-available-rows ()
  "The events screen should use 18 data rows, not 17."
  (let ((data (events-screen-data)))
    (assert data () "Could not find an *ereignisse* screen file")
    (assert (= 18 (screen-repeat-count data)) ()
            "Expected repeat count 18, got ~D" (screen-repeat-count data))))
