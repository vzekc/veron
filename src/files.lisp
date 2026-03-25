;;; -*- Mode: Lisp -*-

;;; File management - loading files between database and filesystem
;;;
;;; Files are stored as BYTEA in the database with a mime_type column.
;;; Text files use mime types like "text/plain; charset=utf-8" and are
;;; encoded/decoded between strings and octets using the charset parameter.

(in-package #:veron)

;;; Mime type helpers

(defun parse-mime-type (mime-type)
  "Parse a mime type string into (values type charset).
TYPE is the base type (e.g. \"text/plain\"), CHARSET is the charset
parameter (e.g. :utf-8) or NIL if absent."
  (let* ((parts (uiop:split-string mime-type :separator ";"))
         (base (string-trim '(#\Space) (first parts)))
         (charset nil))
    (dolist (param (rest parts))
      (let* ((trimmed (string-trim '(#\Space) param))
             (eq-pos (position #\= trimmed)))
        (when (and eq-pos
                   (string-equal "charset"
                                 (string-trim '(#\Space) (subseq trimmed 0 eq-pos))))
          (setf charset (string-trim '(#\Space) (subseq trimmed (1+ eq-pos)))))))
    (values base
            (when charset
              (intern (string-upcase charset) :keyword)))))

(defun text-mime-p (mime-type)
  "Return T if MIME-TYPE denotes a text type."
  (let ((base (parse-mime-type mime-type)))
    (or (uiop:string-prefix-p "text/" base)
        (search "+xml" base)
        (search "+json" base))))

(defun mime-charset (mime-type)
  "Return the charset keyword for a mime type, defaulting to :UTF-8 for text types."
  (multiple-value-bind (base charset) (parse-mime-type mime-type)
    (declare (ignore base))
    (or charset :utf-8)))

(defun text-mime-type (&optional (charset :utf-8))
  "Return a text/plain mime type string with the given charset."
  (format nil "text/plain; charset=~(~A~)" charset))

;;; Encoding

(defun encode-string (string charset)
  "Encode a string to octets using CHARSET."
  (sb-ext:string-to-octets string :external-format charset))

(defun decode-octets (octets charset)
  "Decode octets to a string using CHARSET."
  (sb-ext:octets-to-string octets :external-format charset))

;;; Database operations

(defun create-file (name owner-id &key (content "") (mime-type (text-mime-type)))
  "Create a new file record. Returns the new file ID.
CONTENT is a string for text types, octets for binary types."
  (let ((octets (if (text-mime-p mime-type)
                    (encode-string content (mime-charset mime-type))
                    content)))
    (with-db
      (pomo:query
       "INSERT INTO files (name, mime_type, owner_id, content) VALUES ($1, $2, $3, $4) RETURNING id"
       name mime-type owner-id octets :single))))

(defun save-file-content (file-id content mime-type)
  "Update a file's content and modified timestamp.
CONTENT is a string for text types, octets for binary types."
  (let ((octets (if (text-mime-p mime-type)
                    (encode-string content (mime-charset mime-type))
                    content)))
    (with-db
      (pomo:execute
       "UPDATE files SET content = $1, modified_at = CURRENT_TIMESTAMP WHERE id = $2"
       octets file-id))))

(defun load-file-record (file-id)
  "Load a file record by ID. Returns a plist or NIL.
The :CONTENT value is decoded to a string for text types, raw octets for binary."
  (with-db
    (let ((record (first (pomo:query
                          "SELECT id, name, mime_type, owner_id, content, created_at, modified_at
                           FROM files WHERE id = $1"
                          file-id :plists))))
      (when record
        (let ((mime-type (getf record :mime-type)))
          (when (and (text-mime-p mime-type)
                     (typep (getf record :content) '(simple-array (unsigned-byte 8) (*))))
            (setf (getf record :content)
                  (decode-octets (getf record :content) (mime-charset mime-type)))))
        record))))

(defun delete-file-record (file-id)
  "Delete a file record."
  (with-db
    (pomo:execute "DELETE FROM files WHERE id = $1" file-id)))

(defun user-files (owner-id)
  "List all files owned by a user. Returns plists without content."
  (with-db
    (pomo:query
     "SELECT id, name, mime_type, created_at, modified_at
      FROM files WHERE owner_id = $1 ORDER BY modified_at DESC"
     owner-id :plists)))

(defun file-mime-type (file-id)
  "Return the mime type of a file."
  (with-db
    (pomo:query "SELECT mime_type FROM files WHERE id = $1"
                file-id :single)))

;;; Notes file

(defun user-notes-file-id (user-id)
  "Return the notes file ID for a user, or NIL."
  (with-db
    (let ((result (pomo:query
                   "SELECT notes_file_id FROM users WHERE id = $1"
                   user-id :single)))
      (unless (db-null-p result) result))))

(defvar *notes-welcome-text*
  "Dies ist deine persoenliche Notizdatei.  Du kannst sie nach
Belieben mit dem Editor bearbeiten.  Mit PF1 gibt es Hilfe
zum Editor."
  "Welcome text for newly created notes files.")

(defun ensure-notes-file (user)
  "Ensure the user has a notes file. Creates one if needed. Returns the file ID."
  (let ((file-id (user-notes-file-id (user-id user))))
    (or file-id
        (let ((new-id (create-file "Notizen" (user-id user)
                                   :content *notes-welcome-text*)))
          (with-db
            (pomo:execute
             "UPDATE users SET notes_file_id = $1 WHERE id = $2"
             new-id (user-id user)))
          new-id))))

;;; Filesystem bridge

(defvar *file-tmp-dir* nil
  "Temporary directory for editor file staging.")

(defun file-tmp-dir ()
  "Return the temp directory for file staging, creating it if needed."
  (or *file-tmp-dir*
      (setf *file-tmp-dir*
            (let ((dir (merge-pathnames #P"veron-files/"
                                        (uiop:temporary-directory))))
              (ensure-directories-exist dir)
              dir))))

(defun file-tmp-path (file-id)
  "Return a temp filesystem path for a file ID."
  (merge-pathnames (format nil "~D.txt" file-id) (file-tmp-dir)))

(defun file-to-disk (file-id)
  "Write a file's content from the database to a temp file. Returns the path.
Text files are decoded and written as text. Binary files are written as raw bytes."
  (let* ((record (load-file-record file-id))
         (mime-type (getf record :mime-type))
         (content (getf record :content))
         (path (file-tmp-path file-id)))
    (ensure-directories-exist path)
    (if (text-mime-p mime-type)
        (with-open-file (s path :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format (mime-charset mime-type))
          (write-string (or content "") s))
        (with-open-file (s path :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :element-type '(unsigned-byte 8))
          (when content
            (write-sequence content s))))
    path))

(defun disk-to-file (file-id)
  "Read a temp file's content back into the database.
Text files are read as strings and encoded. Binary files are read as raw bytes."
  (let ((path (file-tmp-path file-id))
        (mime-type (file-mime-type file-id)))
    (when (probe-file path)
      (if (text-mime-p mime-type)
          (let ((content (uiop:read-file-string path)))
            (save-file-content file-id content mime-type))
          (let ((octets (read-binary-file path)))
            (with-db
              (pomo:execute
               "UPDATE files SET content = $1, modified_at = CURRENT_TIMESTAMP WHERE id = $2"
               octets file-id)))))))

(defun read-binary-file (path)
  "Read a file as a vector of octets."
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence octets s)
      octets)))

(defun cleanup-tmp-file (file-id)
  "Remove a temp file for a file ID."
  (let ((path (file-tmp-path file-id)))
    (when (probe-file path)
      (delete-file path))))

;;; Changelog file

(defun changelog-file-id ()
  "Return the changelog file ID, or NIL."
  (with-db
    (let ((result (pomo:query
                   "SELECT id FROM files WHERE name = 'Changelog' AND owner_id IS NULL"
                   :single)))
      (unless (db-null-p result) result))))

(defun ensure-changelog-file ()
  "Ensure the global changelog file exists. Returns its ID."
  (or (changelog-file-id)
      (create-file "Changelog" nil)))

(defun load-changelog-text ()
  "Return the changelog content as a string."
  (let* ((file-id (ensure-changelog-file))
         (record (load-file-record file-id)))
    (or (getf record :content) "")))

(defun save-changelog-text (content)
  "Save changelog text content."
  (let ((file-id (ensure-changelog-file)))
    (save-file-content file-id content (text-mime-type))))
