;;; -*- Mode: Lisp -*-

;;; Tests for Classic Computing (Exhibitron) screens.

(in-package #:veron-tests)

;;; Exhibitron test database helpers

(defun create-exhibitron-test-db (db-name)
  "Create a minimal Exhibitron database with the required schema."
  (pomo:with-connection (admin-db-params)
    (pomo:execute (format nil "CREATE DATABASE ~A" db-name)))
  (pomo:with-connection (test-db-params db-name)
    (pomo:execute
     "CREATE TABLE \"user\" (
        id SERIAL PRIMARY KEY,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        updated_at TIMESTAMPTZ,
        full_name VARCHAR NOT NULL DEFAULT '',
        nickname VARCHAR,
        bio TEXT NOT NULL DEFAULT '',
        is_administrator BOOLEAN NOT NULL DEFAULT FALSE,
        contacts JSONB NOT NULL DEFAULT '{}',
        email VARCHAR NOT NULL DEFAULT '',
        allow_email_contact BOOLEAN NOT NULL DEFAULT FALSE)")
    (pomo:execute
     "CREATE TABLE exhibition (
        id SERIAL PRIMARY KEY,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        updated_at TIMESTAMPTZ,
        key VARCHAR NOT NULL DEFAULT '',
        title VARCHAR NOT NULL DEFAULT '',
        host_match VARCHAR NOT NULL DEFAULT '',
        start_date TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        end_date TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        frozen BOOLEAN NOT NULL DEFAULT FALSE,
        location VARCHAR)")
    (pomo:execute
     "CREATE TABLE exhibitor (
        id SERIAL PRIMARY KEY,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        updated_at TIMESTAMPTZ,
        exhibition_id INTEGER NOT NULL REFERENCES exhibition(id),
        user_id INTEGER NOT NULL REFERENCES \"user\"(id),
        topic VARCHAR)")
    (pomo:execute
     "CREATE TABLE \"table\" (
        id SERIAL PRIMARY KEY,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        updated_at TIMESTAMPTZ,
        exhibitor_id INTEGER REFERENCES exhibitor(id),
        exhibition_id INTEGER NOT NULL REFERENCES exhibition(id),
        number INTEGER NOT NULL)")
    (pomo:execute
     "CREATE TABLE exhibit (
        id SERIAL PRIMARY KEY,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        updated_at TIMESTAMPTZ,
        exhibition_id INTEGER NOT NULL REFERENCES exhibition(id),
        exhibitor_id INTEGER NOT NULL REFERENCES exhibitor(id),
        table_id INTEGER REFERENCES \"table\"(id),
        title VARCHAR NOT NULL DEFAULT '',
        touch_me BOOLEAN NOT NULL DEFAULT FALSE,
        description_id INTEGER,
        attributes JSONB)")))

(defun drop-exhibitron-test-db (db-name)
  "Drop an Exhibitron test database."
  (pomo:with-connection (admin-db-params)
    (ignore-errors
     (pomo:execute
      (format nil "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = '~A'" db-name)))
    (ignore-errors
     (pomo:execute (format nil "DROP DATABASE IF EXISTS ~A" db-name)))))

(defmacro with-exhibitron-test-db ((db-name-var) &body body)
  "Create a temporary Exhibitron database, execute BODY, then drop it.
Temporarily overrides *exhibitron-db-params*."
  (let ((saved-params (gensym "SAVED")))
    `(let ((,db-name-var (generate-db-name "exhibitron_test"))
           (,saved-params veron::*exhibitron-db-params*))
       (create-exhibitron-test-db ,db-name-var)
       (unwind-protect
            (progn
              (setf veron::*exhibitron-db-params* (test-db-params ,db-name-var))
              ,@body)
         (setf veron::*exhibitron-db-params* ,saved-params)
         (pomo:clear-connection-pool)
         (drop-exhibitron-test-db ,db-name-var)))))

(defun seed-exhibitron-tables (db-name)
  "Seed the Exhibitron test DB with an exhibition, exhibitors, and tables.
Creates 3 tables: two occupied (assigned to exhibitors), one unoccupied."
  (pomo:with-connection (test-db-params db-name)
    (pomo:execute
     "INSERT INTO \"user\" (id, full_name, nickname, bio, contacts, email)
      VALUES (1, 'Alice', 'alice', '', '{}', ''),
             (2, 'Bob', 'bob', '', '{}', '')")
    (pomo:execute
     "INSERT INTO exhibition (id, title, key, host_match, start_date, end_date)
      VALUES (1, 'Test Exhibition', 'test', '', NOW(), NOW())")
    (pomo:execute
     "INSERT INTO exhibitor (id, exhibition_id, user_id, topic)
      VALUES (1, 1, 1, 'Retro'),
             (2, 1, 2, 'Vintage')")
    (pomo:execute
     "INSERT INTO \"table\" (id, exhibition_id, exhibitor_id, number)
      VALUES (1, 1, 1, 10),
             (2, 1, 2, 20),
             (3, 1, NULL, 30)")))

;;; Tests

(define-test cc-tische-hides-unoccupied-tables ()
  "Unoccupied tables (no exhibitor) must not appear in the table list."
  (with-exhibitron-test-db (db-name)
    (seed-exhibitron-tables db-name)
    (let ((tables (veron::exhibitron-tables 1 0 100))
          (count (veron::exhibitron-table-count 1)))
      (assert (= 2 count) ()
              "Table count should be 2 (occupied only), got ~D" count)
      (assert (= 2 (length tables)) ()
              "Should return 2 tables, got ~D" (length tables))
      (let ((numbers (mapcar (lambda (tbl) (getf tbl :number)) tables)))
        (assert (member 10 numbers) () "Table 10 (occupied) should be listed")
        (assert (member 20 numbers) () "Table 20 (occupied) should be listed")
        (assert (not (member 30 numbers)) ()
                "Table 30 (unoccupied) should NOT be listed")))))
