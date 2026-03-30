;;; -*- Mode: Lisp -*-

;;; E2E tests for LU configuration admin screens.

(in-package #:veron-tests)

;;; Helper: create an LU with all fields via the UI

(defun create-lu-via-ui (s lu-name &key description disconnect single-instance secure allowed-ips)
  "Navigate to the new LU screen from LU-CONFIG and fill in all fields.
Presses PF5 to save. Uses Tab to navigate between fields."
  (press-pf s 5)
  (assert-screen-contains s "Neue LU Konfiguration")
  ;; Cursor is on lu-name (first writable field), type name
  (erase-eof s)
  (type-text s lu-name)
  ;; Tab to description
  (tab-forward s)
  (when description
    (erase-eof s)
    (type-text s description))
  ;; Tab to disconnect
  (tab-forward s)
  (when disconnect
    (erase-eof s)
    (type-text s disconnect))
  ;; Tab to single-instance
  (tab-forward s)
  (when single-instance
    (erase-eof s)
    (type-text s single-instance))
  ;; Tab to secure
  (tab-forward s)
  (when secure
    (erase-eof s)
    (type-text s secure))
  ;; Tab to allowed-ips
  (tab-forward s)
  (when allowed-ips
    (erase-eof s)
    (type-text s allowed-ips))
  (press-pf s 5))

;;; Helper: press PF3 and confirm discard if prompted

(defun back-with-discard (s)
  "Press PF3 and confirm discard if the unsaved-changes dialog appears."
  (press-pf s 3)
  (when (wait-for-screen-contains s "verwerfen" :timeout 1)
    (press-pf s 5)))

;;; Helper: open an LU for editing from the list screen

(defun open-lu-for-edit (s lu-name)
  "Find LU-NAME in the list and press Enter to edit it."
  (let ((row (loop for r from 3 to 19
                   when (search lu-name (screen-row s r))
                     return r)))
    (assert row () "~A not found in list" lu-name)
    (move-cursor s row 5)
    (press-enter s)
    (assert-on-screen s "LU-CONFIG-EDIT")))

;;; Helper: read field value at a specific content position

(defun read-edit-field (s content-row content-col len)
  "Read a field value from the edit screen at content coordinates.
Content row N maps to physical row N+1 (header at row 0).
Content col maps directly to physical col (data starts at the :from col)."
  (string-trim '(#\Space) (screen-text-at s (1+ content-row) content-col len)))

;;; Full lifecycle: list, create, edit, delete

(define-test e2e-lu-config-lifecycle ()
  (with-veron-app (s :username "luadmin" :password "luadminpass"
                     :roles '(:veron-administrator))
    (login s "luadmin" "luadminpass")
    (assert-on-screen s "MAIN")
    ;; Navigate to System > LU Konfig
    (navigate-to s "LU-CONFIG")
    (assert-on-screen s "LU-CONFIG")
    ;; Default entry should be visible
    (assert-screen-contains s "DEFAULT")
    ;; Create a new LU
    (create-lu-via-ui s "TESTLU01")
    (assert-on-screen s "LU-CONFIG")
    (assert-screen-contains s "TESTLU01")
    ;; Edit the new LU — it's the second entry (row 4 on 24-row, row 4 on 43-row)
    (open-lu-for-edit s "TESTLU01")
    (assert-screen-contains s "TESTLU01")
    ;; Change description — cursor is already on first writable field (description)
    (erase-eof s)
    (type-text s "Updated desc")
    (press-pf s 5)
    (assert-screen-contains s "Gespeichert")
    ;; Go back to list
    (press-pf s 3)
    (assert-on-screen s "LU-CONFIG")
    ;; Edit again and delete
    (open-lu-for-edit s "TESTLU01")
    (press-pf s 9)
    (assert-screen-contains s "loeschen")
    (press-pf s 5)
    (assert (wait-for-screen-contains s "geloescht" :timeout 3)
            () "Should see delete confirmation message")
    ;; Verify the LU is not in the data rows (row 3+), ignoring the message line (row 22)
    (let ((found (loop for r from 3 to 19
                       thereis (search "TESTLU01" (screen-row s r)))))
      (assert (not found) () "Deleted LU should not appear in list data"))))

;;; Default LU cannot be deleted

(define-test e2e-lu-config-default-not-deletable ()
  (with-veron-app (s :username "luadmin2" :password "luadmin2pass"
                     :roles '(:veron-administrator))
    (login s "luadmin2" "luadmin2pass")
    (navigate-to s "LU-CONFIG")
    (assert-on-screen s "LU-CONFIG")
    ;; Select the DEFAULT entry
    (open-lu-for-edit s "DEFAULT")
    (assert-screen-contains s "DEFAULT")
    ;; PF9 should show error about default
    (press-pf s 9)
    (assert-screen-contains s "Standard LU")))

;;; All fields round-trip: create with all fields, verify on edit screen and list

(define-test e2e-lu-config-field-settings ()
  (with-veron-app (s :username "luadmin3" :password "luadmin3pass"
                     :roles '(:veron-administrator))
    (login s "luadmin3" "luadmin3pass")
    (navigate-to s "LU-CONFIG")
    ;; Create LU with all fields populated
    (create-lu-via-ui s "FIELDLU"
                      :description "Test Beschreibung"
                      :disconnect "J"
                      :single-instance "J"
                      :allowed-ips "192.168.1.0/24")
    (assert-on-screen s "LU-CONFIG")
    (assert-screen-contains s "FIELDLU")
    ;; Verify list display: options column shows compact flags
    (let ((row (loop for r from 3 to 19
                     when (search "FIELDLU" (screen-row s r))
                       return r)))
      (assert row () "FIELDLU not found in list")
      (let ((row-text (screen-row s row)))
        (assert (search "DISC" row-text) ()
                "List should show DISC for disconnect=on, row: ~S" row-text)
        (assert (search "SINGLE" row-text) ()
                "List should show SINGLE for single-instance=on, row: ~S" row-text)
        (assert (search "IP:" row-text) ()
                "List should show IP: for allowed-ips set, row: ~S" row-text)))
    ;; Open for editing and verify all fields
    (open-lu-for-edit s "FIELDLU")
    (let ((lu-name (read-edit-field s 3 22 30))
          (description (read-edit-field s 4 22 50))
          (disconnect (read-edit-field s 5 22 1))
          (single-instance (read-edit-field s 6 22 1))
          (secure (read-edit-field s 7 22 1))
          (allowed-ips (read-edit-field s 8 22 50)))
      (assert (string= lu-name "FIELDLU") () "LU name should be FIELDLU, got ~S" lu-name)
      (assert (string= description "Test Beschreibung") ()
              "Description should be Test Beschreibung, got ~S" description)
      (assert (string= disconnect "J") () "Disconnect should be J, got ~S" disconnect)
      (assert (string= single-instance "J") ()
              "Single-instance should be J, got ~S" single-instance)
      (assert (string= secure "N") () "Secure should default to N, got ~S" secure)
      (assert (string= allowed-ips "192.168.1.0/24") ()
              "Allowed-ips should be 192.168.1.0/24, got ~S" allowed-ips))
    ;; Clean up
    (press-pf s 9)
    (assert-screen-contains s "loeschen")
    (press-pf s 5)
    (wait-for-screen-contains s "geloescht" :timeout 3)))

;;; Flag toggle: create with J, edit to N, verify N persists

(define-test e2e-lu-config-flag-toggle ()
  (with-veron-app (s :username "luadmin4" :password "luadmin4pass"
                     :roles '(:veron-administrator))
    (login s "luadmin4" "luadmin4pass")
    (navigate-to s "LU-CONFIG")
    ;; Create with single-instance enabled
    (create-lu-via-ui s "TOGGLELU" :single-instance "J")
    (assert-on-screen s "LU-CONFIG")
    ;; Edit and change single-instance to N
    (open-lu-for-edit s "TOGGLELU")
    (let ((si (read-edit-field s 6 22 1)))
      (assert (string= si "J") () "Single-instance should start as J, got ~S" si))
    ;; Tab to single-instance (cursor on description, Tab to disconnect, Tab to single-instance)
    (tab-forward s)
    (tab-forward s)
    (erase-eof s)
    (type-text s "N")
    (press-pf s 5)
    (assert-screen-contains s "Gespeichert")
    ;; Go back and reopen to verify
    (press-pf s 3)
    (assert-on-screen s "LU-CONFIG")
    (open-lu-for-edit s "TOGGLELU")
    (let ((si (read-edit-field s 6 22 1)))
      (assert (string= si "N") () "Single-instance should be N after toggle, got ~S" si))
    ;; Clean up
    (press-pf s 9)
    (assert-screen-contains s "loeschen")
    (press-pf s 5)
    (wait-for-screen-contains s "geloescht" :timeout 3)))

;;; Flag values: Y and X also enable, N disables

(define-test e2e-lu-config-flag-values ()
  (with-veron-app (s :username "luadmin5" :password "luadmin5pass"
                     :roles '(:veron-administrator))
    (login s "luadmin5" "luadmin5pass")
    (navigate-to s "LU-CONFIG")
    ;; Create with Y for single-instance
    (create-lu-via-ui s "FLAGLU" :single-instance "Y")
    (assert-on-screen s "LU-CONFIG")
    ;; Verify it was saved as enabled (shows J on edit)
    (open-lu-for-edit s "FLAGLU")
    (let ((si (read-edit-field s 6 22 1)))
      (assert (string= si "J") () "Y should enable, display as J, got ~S" si))
    ;; Change to X, verify still enabled
    ;; Tab to single-instance (cursor on description, Tab to disconnect, Tab to single-instance)
    (tab-forward s)
    (tab-forward s)
    (erase-eof s)
    (type-text s "X")
    (press-pf s 5)
    (assert-screen-contains s "Gespeichert")
    (press-pf s 3)
    (open-lu-for-edit s "FLAGLU")
    (let ((si (read-edit-field s 6 22 1)))
      (assert (string= si "J") () "X should enable, display as J, got ~S" si))
    ;; Change to N, verify disabled
    ;; Tab to single-instance (cursor on description, Tab to disconnect, Tab to single-instance)
    (tab-forward s)
    (tab-forward s)
    (erase-eof s)
    (type-text s "N")
    (press-pf s 5)
    (assert-screen-contains s "Gespeichert")
    (press-pf s 3)
    (open-lu-for-edit s "FLAGLU")
    (let ((si (read-edit-field s 6 22 1)))
      (assert (string= si "N") () "N should disable, display as N, got ~S" si))
    ;; Clean up
    (press-pf s 9)
    (assert-screen-contains s "loeschen")
    (press-pf s 5)
    (wait-for-screen-contains s "geloescht" :timeout 3)))

;;; Invalid IP validation

(define-test e2e-lu-config-invalid-ip ()
  (with-veron-app (s :username "luadmin6" :password "luadmin6pass"
                     :roles '(:veron-administrator))
    (login s "luadmin6" "luadmin6pass")
    (navigate-to s "LU-CONFIG")
    ;; Try to create with invalid IP — fill fields manually (create-lu-via-ui expects success)
    (press-pf s 5)
    (assert-screen-contains s "Neue LU Konfiguration")
    ;; Cursor on lu-name, type name
    (erase-eof s)
    (type-text s "BADIPLU")
    ;; Tab to description, disconnect, single-instance, secure, allowed-ips
    (tab-forward s)
    (tab-forward s)
    (tab-forward s)
    (tab-forward s)
    (tab-forward s)
    (erase-eof s)
    (type-text s "not-an-ip")
    (press-pf s 5)
    ;; Should still be on edit screen with error
    (assert-on-screen s "LU-CONFIG-EDIT")
    (assert-screen-contains s "Ungueltige IP")
    (assert-screen-contains s "not-an-ip")
    ;; Press PF3 to go back without saving (confirm discard)
    (back-with-discard s)
    (assert-on-screen s "LU-CONFIG")
    ;; Verify LU was NOT created
    (let ((found (loop for r from 3 to 19
                       thereis (search "BADIPLU" (screen-row s r)))))
      (assert (not found) () "LU with bad IP should not have been created"))))

;;; Valid IP accepted

(define-test e2e-lu-config-valid-ip ()
  (with-veron-app (s :username "luadmin7" :password "luadmin7pass"
                     :roles '(:veron-administrator))
    (login s "luadmin7" "luadmin7pass")
    (navigate-to s "LU-CONFIG")
    ;; Create with valid CIDR
    (create-lu-via-ui s "GOODIPLU" :allowed-ips "10.0.0.0/8")
    (assert-on-screen s "LU-CONFIG")
    (assert-screen-contains s "GOODIPLU")
    ;; Clean up
    (open-lu-for-edit s "GOODIPLU")
    (press-pf s 9)
    (assert-screen-contains s "loeschen")
    (press-pf s 5)
    (wait-for-screen-contains s "geloescht" :timeout 3)))

;;; Mixed valid/invalid IPs - error on the invalid entry

(define-test e2e-lu-config-mixed-ip ()
  (with-veron-app (s :username "luadmin8" :password "luadmin8pass"
                     :roles '(:veron-administrator))
    (login s "luadmin8" "luadmin8pass")
    (navigate-to s "LU-CONFIG")
    ;; Try to create with mixed valid/invalid IPs
    (press-pf s 5)
    (assert-screen-contains s "Neue LU Konfiguration")
    ;; Cursor on lu-name, type name
    (erase-eof s)
    (type-text s "MIXIPLU")
    ;; Tab to description, disconnect, single-instance, secure, allowed-ips
    (tab-forward s)
    (tab-forward s)
    (tab-forward s)
    (tab-forward s)
    (tab-forward s)
    (erase-eof s)
    (type-text s "192.168.1.0/24, bad")
    (press-pf s 5)
    ;; Should show error about "bad"
    (assert-on-screen s "LU-CONFIG-EDIT")
    (assert-screen-contains s "Ungueltige IP")
    (assert-screen-contains s "bad")
    (back-with-discard s)
    (assert-on-screen s "LU-CONFIG")))

;;; IP validation on edit screen

(define-test e2e-lu-config-edit-invalid-ip ()
  (with-veron-app (s :username "luadmin9" :password "luadmin9pass"
                     :roles '(:veron-administrator))
    (login s "luadmin9" "luadmin9pass")
    (navigate-to s "LU-CONFIG")
    ;; Create a valid LU first
    (create-lu-via-ui s "EDITIPLU")
    (assert-on-screen s "LU-CONFIG")
    ;; Edit and try to save with invalid IP
    (open-lu-for-edit s "EDITIPLU")
    ;; Tab to allowed-ips (cursor on description, Tab 4x: disconnect, single-instance, secure, allowed-ips)
    (tab-forward s)
    (tab-forward s)
    (tab-forward s)
    (tab-forward s)
    (erase-eof s)
    (type-text s "garbage")
    (press-pf s 5)
    ;; Should show error, not save
    (assert-screen-contains s "Ungueltige IP")
    (assert-screen-contains s "garbage")
    ;; Go back and verify the LU still has no IPs
    (back-with-discard s)
    (open-lu-for-edit s "EDITIPLU")
    (let ((ips (read-edit-field s 8 22 50)))
      (assert (string= ips "") () "IPs should still be empty, got ~S" ips))
    ;; Clean up
    (press-pf s 9)
    (assert-screen-contains s "loeschen")
    (press-pf s 5)
    (wait-for-screen-contains s "geloescht" :timeout 3)))

;;; Unified screen: lu-name is writable in new mode, read-only in edit mode

(define-test e2e-lu-config-unified-screen ()
  (with-veron-app (s :username "luadmin10" :password "luadmin10pass"
                     :roles '(:veron-administrator))
    (login s "luadmin10" "luadmin10pass")
    (navigate-to s "LU-CONFIG")
    ;; New mode: screen title and lu-name should be writable
    (press-pf s 5)
    (assert-on-screen s "LU-CONFIG-EDIT")
    (assert-screen-contains s "Neue LU Konfiguration")
    (assert-screen-contains s "Anlegen")
    ;; lu-name should be writable (row 4 = physical row for content row 3)
    (assert (row-has-input-field-p s 4) ()
            "lu-name should be writable in new mode")
    (press-pf s 3)
    (assert-on-screen s "LU-CONFIG")
    ;; Create an LU to test edit mode
    (create-lu-via-ui s "UNIFYLU")
    (assert-on-screen s "LU-CONFIG")
    ;; Edit mode: different title, lu-name should be read-only
    (open-lu-for-edit s "UNIFYLU")
    (assert-screen-contains s "LU Konfiguration")
    (assert-screen-contains s "Speichern")
    ;; Clean up
    (press-pf s 9)
    (assert-screen-contains s "loeschen")
    (press-pf s 5)
    (wait-for-screen-contains s "geloescht" :timeout 3)))

;;; Secure flag: create with secure=J, verify on edit and list

(define-test e2e-lu-config-secure-flag ()
  (with-veron-app (s :username "luadmin11" :password "luadmin11pass"
                     :roles '(:veron-administrator))
    (login s "luadmin11" "luadmin11pass")
    (navigate-to s "LU-CONFIG")
    ;; Create LU with secure enabled
    (create-lu-via-ui s "SECURELU" :secure "J")
    (assert-on-screen s "LU-CONFIG")
    (assert-screen-contains s "SECURELU")
    ;; Verify list shows "SSL" flag for secure
    (let ((row (loop for r from 3 to 19
                     when (search "SECURELU" (screen-row s r))
                       return r)))
      (assert row () "SECURELU not found in list")
      (let ((row-text (screen-row s row)))
        (assert (search "SSL" row-text) ()
                "List should show SSL for secure=on, row: ~S" row-text)))
    ;; Open for editing and verify secure field
    (open-lu-for-edit s "SECURELU")
    (let ((secure (read-edit-field s 7 22 1)))
      (assert (string= secure "J") () "Secure should be J, got ~S" secure))
    ;; Toggle secure to N
    ;; Tab from description: disconnect, single-instance, secure
    (tab-forward s)
    (tab-forward s)
    (tab-forward s)
    (erase-eof s)
    (type-text s "N")
    (press-pf s 5)
    (assert-screen-contains s "Gespeichert")
    ;; Verify it persisted
    (press-pf s 3)
    (open-lu-for-edit s "SECURELU")
    (let ((secure (read-edit-field s 7 22 1)))
      (assert (string= secure "N") () "Secure should be N after toggle, got ~S" secure))
    ;; Clean up
    (press-pf s 9)
    (assert-screen-contains s "loeschen")
    (press-pf s 5)
    (wait-for-screen-contains s "geloescht" :timeout 3)))

;;; Secure flag defaults to N for new LU

(define-test e2e-lu-config-secure-default ()
  (with-veron-app (s :username "luadmin12" :password "luadmin12pass"
                     :roles '(:veron-administrator))
    (login s "luadmin12" "luadmin12pass")
    (navigate-to s "LU-CONFIG")
    ;; Create LU without specifying secure
    (create-lu-via-ui s "DEFLU")
    (assert-on-screen s "LU-CONFIG")
    ;; Open for editing and verify secure defaults to N
    (open-lu-for-edit s "DEFLU")
    (let ((secure (read-edit-field s 7 22 1)))
      (assert (string= secure "N") () "Secure should default to N, got ~S" secure))
    ;; Clean up
    (press-pf s 9)
    (assert-screen-contains s "loeschen")
    (press-pf s 5)
    (wait-for-screen-contains s "geloescht" :timeout 3)))

;;; LU name validation: must start with letter, alphanumeric only, auto-upcased

(define-test e2e-lu-config-name-validation ()
  (with-veron-app (s :username "luadmin13" :password "luadmin13pass"
                     :roles '(:veron-administrator))
    (login s "luadmin13" "luadmin13pass")
    (navigate-to s "LU-CONFIG")
    ;; Name starting with digit
    (press-pf s 5)
    (assert-screen-contains s "Neue LU Konfiguration")
    (erase-eof s)
    (type-text s "1BAD")
    (press-pf s 5)
    (assert-on-screen s "LU-CONFIG-EDIT")
    (assert-screen-contains s "Buchstabe")
    (back-with-discard s)
    ;; Name with special characters
    (press-pf s 5)
    (erase-eof s)
    (type-text s "BAD-LU")
    (press-pf s 5)
    (assert-on-screen s "LU-CONFIG-EDIT")
    (assert-screen-contains s "Buchstaben und Ziffern")
    (back-with-discard s)
    ;; Lowercase name should be accepted (auto-upcased)
    (create-lu-via-ui s "testlu")
    (assert-on-screen s "LU-CONFIG")
    (assert-screen-contains s "TESTLU")
    ;; Clean up
    (open-lu-for-edit s "TESTLU")
    (press-pf s 9)
    (assert-screen-contains s "loeschen")
    (press-pf s 5)
    (wait-for-screen-contains s "geloescht" :timeout 3)))
