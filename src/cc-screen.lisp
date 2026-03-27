;;; -*- Mode: Lisp -*-

;;; Classic Computing screens - exhibition data from Exhibitron.

(in-package #:veron)

;;; Helpers

(defparameter *default-exhibition-id* 1
  "Default exhibition ID. CC 2025 = 1, CC 2026 = 2.")

(defun cc-exhibition-id (session)
  "Return the currently selected exhibition ID, defaulting to *default-exhibition-id*."
  (or (lispf:session-property session :cc-exhibition-id)
      (let ((id *default-exhibition-id*))
        (setf (lispf:session-property session :cc-exhibition-id) id)
        id)))

;;; Scrollable text display via repeat fields

(defun show-text-page (lines field-name page-size offset-key)
  "Display a page of LINES in the repeat field FIELD-NAME.
PAGE-SIZE is the number of repeat slots. OFFSET-KEY is a session property
key for tracking the scroll offset. Shows PF7/PF8 as needed."
  (let* ((total (length lines))
         (offset (min (max 0 (or (lispf:session-property lispf:*session* offset-key) 0))
                      (max 0 (- total 1))))
         (page (subseq lines offset (min (+ offset page-size) total)))
         (ctx (lispf:session-context lispf:*session*)))
    (setf (lispf:session-property lispf:*session* offset-key) offset)
    (when page
      (setf (gethash field-name ctx)
            (format nil "~{~A~^~%~}" page)))
    (when (> offset 0)
      (lispf:show-key :pf7 "Vor."))
    (when (< (+ offset page-size) total)
      (lispf:show-key :pf8 "Naech."))))

(defun scroll-text (offset-key page-size total direction)
  "Adjust scroll offset. DIRECTION is :forward or :backward."
  (let ((offset (or (lispf:session-property lispf:*session* offset-key) 0)))
    (setf (lispf:session-property lispf:*session* offset-key)
          (if (eq direction :forward)
              (min (max 0 (- total page-size)) (+ offset page-size))
              (max 0 (- offset page-size))))
    (setf (lispf:session-property lispf:*session* :force-redraw) t))
  :stay)

(defun safe-string (value)
  "Return VALUE as a string, or empty string if null/db-null."
  (if (or (null value) (db-null-p value)) "" value))

(defun wrap-paragraph (text width)
  "Wrap a single paragraph to WIDTH columns, returning a list of lines."
  (let ((lines nil)
        (len (length text))
        (pos 0))
    (loop while (< pos len)
          do (let ((end (min (+ pos width) len)))
               (if (<= end pos)
                   (return)
                   (if (>= end len)
                       (progn (push (subseq text pos) lines)
                              (setf pos len))
                       (let ((break (position #\Space text :end end :from-end t :start pos)))
                         (if (and break (> break pos))
                             (progn (push (subseq text pos break) lines)
                                    (setf pos (1+ break)))
                             (progn (push (subseq text pos end) lines)
                                    (setf pos end))))))))
    (nreverse lines)))

(defun word-wrap (text width)
  "Wrap TEXT to WIDTH columns, respecting existing newlines.
Returns a list of lines."
  (loop for paragraph in (uiop:split-string text :separator '(#\Newline))
        nconc (if (string= paragraph "")
                  (list "")
                  (wrap-paragraph paragraph width))))

;;; CC menu screen - show current exhibition title

(lispf:define-screen-update cc (exhibition-title)
  (unless (exhibitron-configured-p)
    (setf (gethash "%errormsg" (lispf:session-context lispf:*session*))
          "Classic Computing nicht konfiguriert")
    (return-from lispf:prepare-screen :back))
  (let* ((eid (cc-exhibition-id lispf:*session*))
         (detail (when eid (exhibitron-exhibition-detail eid))))
    (when detail
      (setf exhibition-title (safe-string (getf detail :title))))))

;;; Exhibition selection screen

(lispf:define-list-data-getter cc-auswahl (start end)
  (let* ((exhibitions (exhibitron-exhibitions))
         (total (length exhibitions))
         (page (subseq exhibitions start (min end total))))
    (values (loop for e in page
                  collect (list :title (safe-string (getf e :title))
                                :dates (format nil "~A" (format-date (getf e :start-date)))
                                :location (safe-string (getf e :location))))
            total)))

(lispf:define-key-handler cc-auswahl :enter ()
  (let ((index (lispf:selected-list-index)))
    (when index
      (let* ((exhibitions (exhibitron-exhibitions))
             (entry (nth index exhibitions)))
        (when entry
          (setf (lispf:session-property lispf:*session* :cc-exhibition-id)
                (getf entry :id)
                (lispf:session-property lispf:*session* :force-redraw) t)
          :back)))))

;;; Exhibitor list

(lispf:define-list-data-getter cc-aussteller (start end)
  (let* ((eid (cc-exhibition-id lispf:*session*))
         (total (exhibitron-exhibitor-count eid))
         (entries (exhibitron-exhibitors eid start (- end start))))
    (values (loop for e in entries
                  collect (list :name (safe-string (getf e :full-name))
                                :topic (safe-string (getf e :topic))))
            total)))

(lispf:define-key-handler cc-aussteller :enter ()
  (let ((index (lispf:selected-list-index)))
    (when index
      (let* ((eid (cc-exhibition-id lispf:*session*))
             (entries (exhibitron-exhibitors eid index 1))
             (entry (first entries)))
        (when entry
          (let ((eid (getf entry :id)))
            (setf (lispf:session-property lispf:*session* :cc-exhibitor-id) eid
                  (lispf:session-property lispf:*session* :bio-offset) 0)
            (exhibitor-detail-screen eid)))))))

;;; Exhibitor detail — shared helpers

(defun exhibitor-detail-screen (exhibitor-id)
  "Return the appropriate detail screen symbol for EXHIBITOR-ID.
Shows bio view if exhibitor has a bio, otherwise exhibits view."
  (let ((detail (exhibitron-exhibitor-detail exhibitor-id)))
    (when detail
      (let ((bio (getf detail :bio)))
        (if (and bio (not (db-null-p bio)) (plusp (length bio)))
            'cc-aussteller-detail
            'cc-aussteller-exponate)))))

(defun exhibitor-header-data ()
  "Return (values detail tables-string) for the current exhibitor."
  (let* ((exhibitor-id (lispf:session-property lispf:*session* :cc-exhibitor-id))
         (detail (when exhibitor-id (exhibitron-exhibitor-detail exhibitor-id))))
    (when detail
      (let ((tables (exhibitron-exhibitor-tables exhibitor-id)))
        (values detail
                (format nil "~{~D~^, ~}"
                        (mapcar (lambda (tbl) (getf tbl :number)) tables)))))))

;;; Exhibitor detail — bio view

(lispf:define-screen-update cc-aussteller-detail (name topic table-list)
  (multiple-value-bind (detail tables-str) (exhibitor-header-data)
    (when detail
      (setf name (safe-string (getf detail :full-name))
            topic (safe-string (getf detail :topic))
            table-list tables-str)
      (let ((bio-text (getf detail :bio)))
        (when (and bio-text (not (db-null-p bio-text)) (plusp (length bio-text)))
          (let ((lines (word-wrap bio-text 75)))
            (setf (lispf:session-property lispf:*session* :bio-lines) lines)
            (show-text-page lines "bio-line" 14 :bio-offset))))
      (let ((exhibit-count (getf detail :exhibit-count)))
        (when (and exhibit-count (plusp exhibit-count))
          (lispf:show-key :pf4 "Exponate"))))))

(lispf:define-key-handler cc-aussteller-detail :pf7 ()
  (let ((lines (lispf:session-property lispf:*session* :bio-lines)))
    (when lines (scroll-text :bio-offset 14 (length lines) :backward))))

(lispf:define-key-handler cc-aussteller-detail :pf8 ()
  (let ((lines (lispf:session-property lispf:*session* :bio-lines)))
    (when lines (scroll-text :bio-offset 14 (length lines) :forward))))

(lispf:define-key-handler cc-aussteller-detail :pf4 ()
  (setf (lispf:session-current-screen lispf:*session*) 'cc-aussteller-exponate
        (lispf:session-property lispf:*session* :force-redraw) t)
  :stay)

;;; Exhibitor detail — exhibits view

(lispf:define-screen-update cc-aussteller-exponate (name topic table-list)
  (multiple-value-bind (detail tables-str) (exhibitor-header-data)
    (when detail
      (setf name (safe-string (getf detail :full-name))
            topic (safe-string (getf detail :topic))
            table-list tables-str)
      (let ((bio (getf detail :bio)))
        (when (and bio (not (db-null-p bio)) (plusp (length bio)))
          (lispf:show-key :pf4 "Info"))))))

(lispf:define-list-data-getter cc-aussteller-exponate (start end)
  (let* ((exhibitor-id (lispf:session-property lispf:*session* :cc-exhibitor-id))
         (exhibits (when exhibitor-id (exhibitron-exhibitor-exhibits exhibitor-id)))
         (total (length exhibits))
         (page (subseq exhibits start (min end total))))
    (values (loop for ex in page
                  for tbl = (getf ex :table-number)
                  collect (list :exhibit-title (safe-string (getf ex :title))
                                :exhibit-table (if (or (null tbl) (db-null-p tbl))
                                                   ""
                                                   (format nil "~D" tbl))))
            total)))

(lispf:define-key-handler cc-aussteller-exponate :enter ()
  (let ((index (lispf:selected-list-index)))
    (when index
      (let* ((exhibitor-id (lispf:session-property lispf:*session* :cc-exhibitor-id))
             (exhibits (when exhibitor-id (exhibitron-exhibitor-exhibits exhibitor-id)))
             (entry (nth index exhibits)))
        (when entry
          (setf (lispf:session-property lispf:*session* :cc-exhibit-id)
                (getf entry :id)
                (lispf:session-property lispf:*session* :desc-offset) 0)
          'cc-exponat-detail)))))

(lispf:define-key-handler cc-aussteller-exponate :pf4 ()
  (setf (lispf:session-current-screen lispf:*session*) 'cc-aussteller-detail
        (lispf:session-property lispf:*session* :force-redraw) t)
  :stay)

;;; Exhibit list

(lispf:define-list-data-getter cc-exponate (start end)
  (let* ((eid (cc-exhibition-id lispf:*session*))
         (total (exhibitron-exhibit-count eid))
         (entries (exhibitron-exhibits eid start (- end start))))
    (values (loop for e in entries
                  for tbl = (getf e :table-number)
                  collect (list :title (safe-string (getf e :title))
                                :exhibitor-name (safe-string (getf e :exhibitor-name))
                                :table-number (if (or (null tbl) (db-null-p tbl))
                                                  ""
                                                  (format nil "~D" tbl))))
            total)))

(lispf:define-key-handler cc-exponate :enter ()
  (let ((index (lispf:selected-list-index)))
    (when index
      (let* ((eid (cc-exhibition-id lispf:*session*))
             (entries (exhibitron-exhibits eid index 1))
             (entry (first entries)))
        (when entry
          (setf (lispf:session-property lispf:*session* :cc-exhibit-id)
                (getf entry :id)
                (lispf:session-property lispf:*session* :desc-offset) 0)
          'cc-exponat-detail)))))

;;; Exhibit detail — shared header helper

(defun exhibit-detail-data ()
  "Return (values detail table-string) for the current exhibit."
  (let* ((exhibit-id (lispf:session-property lispf:*session* :cc-exhibit-id))
         (detail (when exhibit-id (exhibitron-exhibit-detail exhibit-id))))
    (when detail
      (let* ((tbl (getf detail :table-number))
             (table-str (if (or (null tbl) (db-null-p tbl))
                            (let ((tables (exhibitron-exhibitor-tables
                                           (getf detail :exhibitor-id))))
                              (format nil "~{~D~^, ~}"
                                      (mapcar (lambda (tbl) (getf tbl :number))
                                              tables)))
                            (format nil "~D" tbl))))
        (values detail table-str)))))

;;; Exhibit detail — description view

(lispf:define-screen-update cc-exponat-detail
    (title exhibitor-name table-number touch-me)
  (multiple-value-bind (detail table-str) (exhibit-detail-data)
    (when detail
      (setf title (safe-string (getf detail :title))
            exhibitor-name (safe-string (getf detail :exhibitor-name))
            table-number table-str
            touch-me (if (getf detail :touch-me) "Ja" "Nein"))
      (let ((desc (getf detail :description)))
        (when (and desc (not (db-null-p desc)) (plusp (length desc)))
          (let ((lines (word-wrap desc 75)))
            (setf (lispf:session-property lispf:*session* :desc-lines) lines)
            (show-text-page lines "desc-line" 13 :desc-offset))))
      (let ((attrs (exhibitron-exhibit-attributes
                    (lispf:session-property lispf:*session* :cc-exhibit-id))))
        (when attrs
          (lispf:show-key :pf4 "Attribute"))))))

(lispf:define-key-handler cc-exponat-detail :pf7 ()
  (let ((lines (lispf:session-property lispf:*session* :desc-lines)))
    (when lines (scroll-text :desc-offset 13 (length lines) :backward))))

(lispf:define-key-handler cc-exponat-detail :pf8 ()
  (let ((lines (lispf:session-property lispf:*session* :desc-lines)))
    (when lines (scroll-text :desc-offset 13 (length lines) :forward))))

(lispf:define-key-handler cc-exponat-detail :pf4 ()
  (setf (lispf:session-current-screen lispf:*session*) 'cc-exponat-attr
        (lispf:session-property lispf:*session* :force-redraw) t)
  :stay)

;;; Exhibit detail — attributes view

(lispf:define-screen-update cc-exponat-attr
    (title exhibitor-name table-number touch-me)
  (multiple-value-bind (detail table-str) (exhibit-detail-data)
    (when detail
      (setf title (safe-string (getf detail :title))
            exhibitor-name (safe-string (getf detail :exhibitor-name))
            table-number table-str
            touch-me (if (getf detail :touch-me) "Ja" "Nein"))
      (let ((attrs (exhibitron-exhibit-attributes
                    (lispf:session-property lispf:*session* :cc-exhibit-id)))
            (ctx (lispf:session-context lispf:*session*)))
        (when attrs
          (setf (gethash "attr-name" ctx)
                (format nil "~{~A~^~%~}" (mapcar #'first attrs))
                (gethash "attr-sep" ctx)
                (format nil "~{~A~^~%~}" (make-list (length attrs) :initial-element ":"))
                (gethash "attr-value" ctx)
                (format nil "~{~A~^~%~}" (mapcar #'second attrs)))))
      (let ((desc (getf detail :description)))
        (when (and desc (not (db-null-p desc)) (plusp (length desc)))
          (lispf:show-key :pf4 "Beschreibung"))))))

(lispf:define-key-handler cc-exponat-attr :pf4 ()
  (setf (lispf:session-current-screen lispf:*session*) 'cc-exponat-detail
        (lispf:session-property lispf:*session* :force-redraw) t)
  :stay)

;;; Table list

(lispf:define-list-data-getter cc-tische (start end)
  (let* ((eid (cc-exhibition-id lispf:*session*))
         (total (exhibitron-table-count eid))
         (entries (exhibitron-tables eid start (- end start))))
    (values (loop for e in entries
                  for ec = (getf e :exhibit-count)
                  collect (list :table-number (format nil "~D" (getf e :number))
                                :exhibitor-name (safe-string (getf e :exhibitor-name))
                                :exhibit-count (if (and ec (plusp ec))
                                                   (format nil "~D" ec)
                                                   "")))
            total)))

(lispf:define-key-handler cc-tische :enter ()
  (let ((index (lispf:selected-list-index)))
    (when index
      (let* ((eid (cc-exhibition-id lispf:*session*))
             (entries (exhibitron-tables eid index 1))
             (entry (first entries))
             (exhibitor-id (when entry (getf entry :exhibitor-id))))
        (when (and exhibitor-id (not (db-null-p exhibitor-id)))
          (setf (lispf:session-property lispf:*session* :cc-exhibitor-id)
                exhibitor-id
                (lispf:session-property lispf:*session* :bio-offset) 0)
          (exhibitor-detail-screen exhibitor-id))))))
