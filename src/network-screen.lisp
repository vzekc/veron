;;; -*- Mode: Lisp -*-

;;; Network status screen - shows retro computers from RetroStar.

(in-package #:veron)

;;; Network host list

(lispf:define-list-data-getter network (start end)
  (let* ((show-all (lispf:session-property lispf:*session* :show-all-hosts))
         (total (if show-all
                    (retrostar-all-host-count)
                    (retrostar-active-host-count)))
         (entries (if show-all
                      (retrostar-all-hosts start (- end start))
                      (retrostar-active-hosts start (- end start)))))
    (values (loop for e in entries
                  for hw = (getf e :hardware)
                  for vn = (getf e :vendor)
                  collect (list :owner (or (getf e :owner) "")
                                :name (let ((n (getf e :name)))
                                        (if (db-null-p n) "" (or n "")))
                                :hardware (if (or (null hw) (db-null-p hw)) "" hw)
                                :vendor (if (or (null vn) (db-null-p vn)) "" vn)
                                :protocols (or (format-protocols (getf e :protocols)) "")))
            total)))

(lispf:define-screen-update network ()
  (let ((show-all (lispf:session-property lispf:*session* :show-all-hosts)))
    (if show-all
        (lispf:show-key :pf5 "Aktive")
        (lispf:show-key :pf5 "Alle"))))

(lispf:define-key-handler network :pf5 ()
  (setf (lispf:session-property lispf:*session* :show-all-hosts)
        (not (lispf:session-property lispf:*session* :show-all-hosts))
        (lispf:list-offset lispf:*session* 'network) 0
        (lispf:session-property lispf:*session* :force-redraw) t)
  :stay)

(lispf:define-key-handler network :enter ()
  (let ((index (lispf:selected-list-index)))
    (when index
      (let* ((show-all (lispf:session-property lispf:*session* :show-all-hosts))
             (entries (if show-all
                         (retrostar-all-hosts index 1)
                         (retrostar-active-hosts index 1)))
             (entry (first entries)))
        (when entry
          (setf (lispf:session-property lispf:*session* :network-detail-mac)
                (getf entry :mac-address))
          'network-detail)))))

;;; Network host detail

(lispf:define-screen-update network-detail
    (owner name mac hardware software vendor decnet last-seen)
  (let* ((mac-addr (lispf:session-property lispf:*session* :network-detail-mac))
         (entry (when mac-addr (retrostar-host-detail mac-addr))))
    (when entry
      (let ((hw (getf entry :hardware))
            (sw (getf entry :software))
            (vn (getf entry :vendor))
            (ls (getf entry :last-seen))
            (mac-str (format nil "~A" (getf entry :mac-address))))
        (setf owner (or (getf entry :owner) "")
              name (let ((n (getf entry :name)))
                     (if (db-null-p n) "" (or n "")))
              mac mac-str
              hardware (if (or (null hw) (db-null-p hw)) "" hw)
              software (if (or (null sw) (db-null-p sw)) "" sw)
              vendor (if (or (null vn) (db-null-p vn)) "" vn)
              decnet (or (mac-to-decnet mac-str) "")
              last-seen (if (or (null ls) (db-null-p ls)) "" (format-datetime ls))))
      ;; Fill protocol detail repeat fields directly in the session context
      (let ((protos (retrostar-host-protocols-detail (getf entry :protocols)))
            (ctx (lispf:session-context lispf:*session*)))
        (dotimes (i 7)
          (if (< i (length protos))
              (setf (gethash (format nil "proto-name.~D" i) ctx) (first (nth i protos))
                    (gethash (format nil "proto-desc.~D" i) ctx) (second (nth i protos)))
              (setf (gethash (format nil "proto-name.~D" i) ctx) ""
                    (gethash (format nil "proto-desc.~D" i) ctx) "")))))))
