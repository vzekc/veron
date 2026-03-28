;;; -*- Mode: Lisp -*-

;;; Read-only access to the Exhibitron database for Classic Computing screens.

(in-package #:veron)

;;; Database connection

(defvar *exhibitron-db-params* nil
  "Postmodern connection parameters for the Exhibitron database.")

(defun exhibitron-configured-p ()
  "Return T if the exhibitron database is configured via environment variables."
  (not (null (uiop:getenv "EXHIBITRON_DB_NAME"))))

(defun exhibitron-db-params ()
  (or *exhibitron-db-params*
      (setf *exhibitron-db-params*
            (list (env "EXHIBITRON_DB_NAME")
                  (env "EXHIBITRON_DB_USER" (env "VERON_DB_USER"))
                  (env "EXHIBITRON_DB_PASSWORD" (env "VERON_DB_PASSWORD"))
                  (env "EXHIBITRON_DB_HOST" (env "VERON_DB_HOST"))
                  :port (parse-integer
                         (env "EXHIBITRON_DB_PORT" (env "VERON_DB_PORT")))))))

(defmacro with-exhibitron-db (&body body)
  `(pomo:with-connection (exhibitron-db-params)
     ,@body))

;;; Exhibition queries

(defun exhibitron-exhibitions ()
  "Return all exhibitions ordered by start date descending."
  (with-exhibitron-db
    (pomo:query
     "SELECT id, title, start_date, end_date, location
      FROM exhibition
      ORDER BY start_date DESC"
     :plists)))

(defun exhibitron-exhibition-count ()
  (with-exhibitron-db
    (pomo:query "SELECT COUNT(*) FROM exhibition" :single)))

(defun exhibitron-latest-exhibition-id ()
  "Return the ID of the exhibition with the latest start date."
  (with-exhibitron-db
    (pomo:query
     "SELECT id FROM exhibition ORDER BY start_date DESC LIMIT 1"
     :single)))

(defun exhibitron-exhibition-detail (exhibition-id)
  "Return details for a single exhibition."
  (with-exhibitron-db
    (first
     (pomo:query
      "SELECT e.id, e.title, e.start_date, e.end_date, e.location,
              (SELECT COUNT(*) FROM exhibitor ex WHERE ex.exhibition_id = e.id) AS exhibitor_count,
              (SELECT COUNT(*) FROM exhibit exh WHERE exh.exhibition_id = e.id) AS exhibit_count,
              (SELECT COUNT(*) FROM \"table\" t WHERE t.exhibition_id = e.id) AS table_count
       FROM exhibition e
       WHERE e.id = $1"
      exhibition-id :plists))))

;;; Exhibitor queries

(defun exhibitron-exhibitors (exhibition-id start count)
  "Return exhibitors for an exhibition, ordered by name."
  (with-exhibitron-db
    (pomo:query
     "SELECT er.id, COALESCE(NULLIF(u.full_name, ''), u.nickname, '') AS full_name,
             er.topic
      FROM exhibitor er
      JOIN \"user\" u ON er.user_id = u.id
      WHERE er.exhibition_id = $1
      ORDER BY LOWER(COALESCE(NULLIF(u.full_name, ''), u.nickname, ''))
      LIMIT $2 OFFSET $3"
     exhibition-id count start :plists)))

(defun exhibitron-exhibitor-count (exhibition-id)
  (with-exhibitron-db
    (pomo:query
     "SELECT COUNT(*) FROM exhibitor WHERE exhibition_id = $1"
     exhibition-id :single)))

(defun exhibitron-exhibitor-detail (exhibitor-id)
  "Return exhibitor detail with exhibit and table counts."
  (with-exhibitron-db
    (first
     (pomo:query
      "SELECT er.id, COALESCE(NULLIF(u.full_name, ''), u.nickname, '') AS full_name,
              er.topic, u.bio,
              (SELECT COUNT(*) FROM exhibit ex WHERE ex.exhibitor_id = er.id) AS exhibit_count,
              (SELECT COUNT(*) FROM \"table\" t WHERE t.exhibitor_id = er.id) AS table_count
       FROM exhibitor er
       JOIN \"user\" u ON er.user_id = u.id
       WHERE er.id = $1"
      exhibitor-id :plists))))

(defun exhibitron-exhibitor-exhibits (exhibitor-id)
  "Return exhibits for a given exhibitor."
  (with-exhibitron-db
    (pomo:query
     "SELECT ex.id, ex.title, ex.touch_me, t.number AS table_number
      FROM exhibit ex
      LEFT JOIN \"table\" t ON ex.table_id = t.id
      WHERE ex.exhibitor_id = $1
      ORDER BY ex.title"
     exhibitor-id :plists)))

(defun exhibitron-exhibitor-tables (exhibitor-id)
  "Return tables assigned to a given exhibitor."
  (with-exhibitron-db
    (pomo:query
     "SELECT t.number
      FROM \"table\" t
      WHERE t.exhibitor_id = $1
      ORDER BY t.number"
     exhibitor-id :plists)))

;;; Exhibit queries

(defun exhibitron-exhibits (exhibition-id start count)
  "Return exhibits for an exhibition, ordered by title."
  (with-exhibitron-db
    (pomo:query
     "SELECT ex.id, ex.title, ex.touch_me,
             COALESCE(NULLIF(u.full_name, ''), u.nickname, '') AS exhibitor_name,
             t.number AS table_number
      FROM exhibit ex
      JOIN exhibitor er ON ex.exhibitor_id = er.id
      JOIN \"user\" u ON er.user_id = u.id
      LEFT JOIN \"table\" t ON ex.table_id = t.id
      WHERE ex.exhibition_id = $1
      ORDER BY LOWER(ex.title)
      LIMIT $2 OFFSET $3"
     exhibition-id count start :plists)))

(defun exhibitron-exhibit-count (exhibition-id)
  (with-exhibitron-db
    (pomo:query
     "SELECT COUNT(*) FROM exhibit WHERE exhibition_id = $1"
     exhibition-id :single)))

(defun exhibitron-exhibit-detail (exhibit-id)
  "Return full details for a single exhibit."
  (with-exhibitron-db
    (first
     (pomo:query
      "SELECT ex.id, ex.title, ex.touch_me,
              er.id AS exhibitor_id,
              COALESCE(NULLIF(u.full_name, ''), u.nickname, '') AS exhibitor_name,
              t.number AS table_number,
              regexp_replace(
                regexp_replace(
                  regexp_replace(d.html, '</p><p>', E'\\n', 'g'),
                  '<br\\s*/?>', E'\\n', 'g'),
                '<[^>]+>', '', 'g') AS description
       FROM exhibit ex
       JOIN exhibitor er ON ex.exhibitor_id = er.id
       JOIN \"user\" u ON er.user_id = u.id
       LEFT JOIN \"table\" t ON ex.table_id = t.id
       LEFT JOIN document d ON ex.description_id = d.id
       WHERE ex.id = $1"
      exhibit-id :plists))))

(defun exhibitron-exhibit-attributes (exhibit-id)
  "Return attributes for an exhibit as a list of (name value) pairs."
  (with-exhibitron-db
    (pomo:query
     "SELECT elem->>0 AS name, elem->>1 AS value
      FROM exhibit,
           jsonb_array_elements(attributes) AS elem
      WHERE id = $1 AND attributes IS NOT NULL"
     exhibit-id :rows)))

;;; Table queries

(defun exhibitron-tables (exhibition-id start count)
  "Return tables for an exhibition, ordered by number."
  (with-exhibitron-db
    (pomo:query
     "SELECT t.id, t.number, er.id AS exhibitor_id,
             COALESCE(NULLIF(u.full_name, ''), u.nickname, '') AS exhibitor_name,
             (SELECT COUNT(*) FROM exhibit ex WHERE ex.table_id = t.id) AS exhibit_count
      FROM \"table\" t
      LEFT JOIN exhibitor er ON t.exhibitor_id = er.id
      LEFT JOIN \"user\" u ON er.user_id = u.id
      WHERE t.exhibition_id = $1 AND t.exhibitor_id IS NOT NULL
      ORDER BY t.number
      LIMIT $2 OFFSET $3"
     exhibition-id count start :plists)))

(defun exhibitron-table-count (exhibition-id)
  (with-exhibitron-db
    (pomo:query
     "SELECT COUNT(*) FROM \"table\" WHERE exhibition_id = $1 AND exhibitor_id IS NOT NULL"
     exhibition-id :single)))
