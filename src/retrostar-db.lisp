;;; -*- Mode: Lisp -*-

;;; Read-only access to the RetroStar database for network status display.

(in-package #:veron)

;;; Database connection

(defvar *retrostar-db-params* nil
  "Postmodern connection parameters for the RetroStar database.")

(defun retrostar-db-params ()
  (or *retrostar-db-params*
      (setf *retrostar-db-params*
            (let ((veron-params (db-params)))
              (list* "retrostar"
                     (second veron-params)
                     (third veron-params)
                     (fourth veron-params)
                     (nthcdr 4 veron-params))))))

(defmacro with-retrostar-db (&body body)
  `(pomo:with-connection (retrostar-db-params)
     ,@body))

;;; Protocol name cache

(defvar *protocol-names* nil
  "Cached alist of protocol number to short name.")

(defun load-protocol-names ()
  "Load protocol names from the RetroStar database."
  (with-retrostar-db
    (pomo:query "SELECT number, name FROM protocol ORDER BY number")))

(defun protocol-names ()
  (or *protocol-names*
      (setf *protocol-names* (load-protocol-names))))

(defun protocol-name (number)
  "Return the short name for a protocol number, or hex if unknown."
  (let* ((entry (assoc number (protocol-names)))
         (name (when entry (second entry))))
    (if (and name (not (db-null-p name)))
        name
        (format nil "0x~4,'0X" number))))

(defun format-protocols (protocol-array)
  "Format a protocol integer array as space-separated short names."
  (when (and protocol-array (not (db-null-p protocol-array)))
    (let ((protos (coerce protocol-array 'list)))
      (format nil "~{~A~^ ~}" (mapcar #'protocol-name protos)))))

;;; DECnet address from MAC

(defun mac-to-decnet (mac-string)
  "Convert a DECnet MAC address (AA:00:04:00:xx:yy) to area.node, or NIL.
The DECnet address is encoded little-endian: addr = yy*256 + xx,
where addr = area*1024 + node."
  (let ((parts (uiop:split-string mac-string :separator ":")))
    (when (and (= (length parts) 6)
               (string-equal (first parts) "aa")
               (string-equal (second parts) "00")
               (string-equal (third parts) "04")
               (string-equal (fourth parts) "00"))
      (let* ((lo (parse-integer (fifth parts) :radix 16))
             (hi (parse-integer (sixth parts) :radix 16))
             (addr (+ (* hi 256) lo))
             (area (ash addr -10))
             (node (logand addr #x3FF)))
        (format nil "~D.~D" area node)))))

;;; Host queries

(defun retrostar-active-hosts (start count)
  "Return active (seen in last 5 min) non-blacklisted hosts."
  (with-retrostar-db
    (pomo:query
     "SELECT h.mac_address, u.name AS owner, h.name, h.hardware, h.software,
             h.protocols, h.last_seen,
             ev.vendor
      FROM host h
      JOIN \"user\" u ON h.user_id = u.id
      LEFT JOIN ethernet_vendor ev ON TRUNC(h.mac_address) = ev.mac_prefix
      WHERE h.last_seen > NOW() - INTERVAL '5 minutes'
        AND NOT h.blacklisted
      ORDER BY u.name, h.mac_address
      LIMIT $1 OFFSET $2"
     count start :plists)))

(defun retrostar-active-host-count ()
  (with-retrostar-db
    (pomo:query
     "SELECT COUNT(*) FROM host
      WHERE last_seen > NOW() - INTERVAL '5 minutes'
        AND NOT blacklisted"
     :single)))

(defun retrostar-all-hosts (start count)
  "Return all non-blacklisted hosts."
  (with-retrostar-db
    (pomo:query
     "SELECT h.mac_address, u.name AS owner, h.name, h.hardware, h.software,
             h.protocols, h.last_seen,
             ev.vendor
      FROM host h
      JOIN \"user\" u ON h.user_id = u.id
      LEFT JOIN ethernet_vendor ev ON TRUNC(h.mac_address) = ev.mac_prefix
      WHERE NOT h.blacklisted
      ORDER BY u.name, h.mac_address
      LIMIT $1 OFFSET $2"
     count start :plists)))

(defun retrostar-all-host-count ()
  (with-retrostar-db
    (pomo:query
     "SELECT COUNT(*) FROM host WHERE NOT blacklisted"
     :single)))

(defun retrostar-host-detail (mac-address)
  "Return full details for a single host."
  (with-retrostar-db
    (first
     (pomo:query
      "SELECT h.mac_address, u.name AS owner, h.name, h.hardware, h.software,
              h.protocols, h.last_seen, h.description,
              ev.vendor
       FROM host h
       JOIN \"user\" u ON h.user_id = u.id
       LEFT JOIN ethernet_vendor ev ON TRUNC(h.mac_address) = ev.mac_prefix
       WHERE h.mac_address = $1::macaddr"
      mac-address :plists))))

(defun retrostar-host-protocols-detail (protocol-array)
  "Return list of (name description) for each protocol number."
  (when (and protocol-array (not (db-null-p protocol-array)))
    (let ((protos (coerce protocol-array 'list)))
      (with-retrostar-db
        (loop for num in protos
              for row = (first (pomo:query
                                "SELECT name, description FROM protocol WHERE number = $1"
                                num :rows))
              collect (if row
                         (list (first row) (second row))
                         (list (format nil "0x~4,'0X" num) "")))))))

;;; DECnet queries

(defun retrostar-decnet-blocks (start count)
  "Return DECnet blocks with owner and host names."
  (with-retrostar-db
    (pomo:query
     "SELECT b.id, u.name AS owner, b.start_node, b.end_node,
             COALESCE(
               STRING_AGG(dh.name || '(' || dh.node_number || ')', ', '
                          ORDER BY dh.node_number),
               '') AS hosts
      FROM decnet_block b
      JOIN \"user\" u ON b.user_id = u.id
      LEFT JOIN decnet_host dh ON dh.block_id = b.id
      GROUP BY b.id, u.name, b.start_node, b.end_node
      ORDER BY b.start_node
      LIMIT $1 OFFSET $2"
     count start :plists)))

(defun retrostar-decnet-block-count ()
  (with-retrostar-db
    (pomo:query "SELECT COUNT(*) FROM decnet_block" :single)))

;;; Event queries

(defun retrostar-events (start count)
  "Return recent events, newest first."
  (with-retrostar-db
    (pomo:query
     "SELECT timestamp AT TIME ZONE 'UTC' AS timestamp, type, message
      FROM event
      ORDER BY timestamp DESC
      LIMIT $1 OFFSET $2"
     count start :plists)))

(defun retrostar-event-count ()
  (with-retrostar-db
    (pomo:query "SELECT COUNT(*) FROM event" :single)))
