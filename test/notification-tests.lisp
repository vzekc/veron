;;; -*- Mode: Lisp -*-

;;; E2E tests for the notification system.

(in-package #:veron-tests)

;;; Helper: set up notification settings for a user

(defun enable-notifications (user-id &key ntfy local (topic "test-topic"))
  "Enable notification settings for all events."
  (veron::with-db
    (when topic
      (pomo:execute "UPDATE users SET ntfy_topic = $2 WHERE id = $1" user-id topic))
    (dolist (event '("guestbook" "login" "logout"))
      (pomo:execute
       "INSERT INTO notification_settings (user_id, event, ntfy, local)
        VALUES ($1, $2, $3, $4)
        ON CONFLICT (user_id, event) DO UPDATE SET ntfy = $3, local = $4"
       user-id event (if ntfy t nil) (if local t nil)))))

;;; Settings screen: navigate, toggle, save, verify persistence

(define-test e2e-notification-settings-save ()
  (with-veron-app (s :username "notifuser1" :password "notifpass1")
    (login s "notifuser1" "notifpass1")
    (navigate-to s "NOTIFICATIONS")
    (assert-on-screen s "NOTIFICATIONS")
    (assert-screen-contains s "Benachrichtigungseinstellungen")
    ;; Cursor starts on topic field — fill in ntfy topic
    (erase-eof s)
    (type-text s "my-test-topic")
    ;; Tab to beep
    (tab-forward s)
    (erase-eof s)
    (type-text s "x")
    ;; Tab to ntfy-guestbook
    (tab-forward s)
    (erase-eof s)
    (type-text s "x")
    ;; Tab to local-guestbook
    (tab-forward s)
    (erase-eof s)
    (type-text s "x")
    ;; Skip ntfy-login, local-login
    (tab-forward s)
    (tab-forward s)
    ;; Tab to ntfy-logout
    (tab-forward s)
    (erase-eof s)
    (type-text s "x")
    ;; Skip local-logout
    (press-pf s 5)
    (assert-screen-contains s "Gespeichert")
    ;; Leave and come back to verify persistence
    (press-pf s 3)
    (navigate-to s "NOTIFICATIONS")
    (assert-on-screen s "NOTIFICATIONS")
    (assert-screen-contains s "my-test-topic")
    (assert-screen-contains s "x")))

;;; Settings screen: PF9 toggles all on/off

(define-test e2e-notification-settings-toggle-all ()
  (with-veron-app (s :username "notifuser2" :password "notifpass2")
    (login s "notifuser2" "notifpass2")
    ;; Pre-enable some settings so PF9 has something to toggle off
    (veron::with-db
      (let ((uid (pomo:query "SELECT id FROM users WHERE name = 'notifuser2'" :single)))
        (enable-notifications uid :local t)))
    (navigate-to s "NOTIFICATIONS")
    ;; Settings should show x for local checkboxes
    (assert-screen-contains s "x")
    ;; PF9 toggles all off in-memory, PF5 saves
    (press-pf s 9)
    ;; Fill in a dummy topic so PF5 doesn't complain (ntfy checkboxes were toggled off)
    ;; Actually PF9 cleared all ntfy too, so no topic needed
    (press-pf s 5)
    (assert-screen-contains s "Gespeichert")
    ;; Verify in DB: all settings should be false
    (veron::with-db
      (let ((uid (pomo:query "SELECT id FROM users WHERE name = 'notifuser2'" :single)))
        (let ((any-on (pomo:query
                       "SELECT COUNT(*) FROM notification_settings WHERE user_id = $1 AND (local = TRUE OR ntfy = TRUE)"
                       uid :single)))
          (assert (zerop any-on) ()
                  "All settings should be off after toggle, got ~D" any-on))))))

;;; Settings screen: ntfy without topic shows error

(define-test e2e-notification-settings-ntfy-no-topic ()
  (with-veron-app (s :username "notifuser3" :password "notifpass3")
    (login s "notifuser3" "notifpass3")
    (navigate-to s "NOTIFICATIONS")
    ;; Enable ntfy-guestbook without topic
    ;; Cursor on topic — skip to beep, then ntfy-guestbook
    (tab-forward s)
    (tab-forward s)
    (erase-eof s)
    (type-text s "x")
    (press-pf s 5)
    (assert-screen-contains s "ntfy-Topic")))

;;; Notification review screen: accessible and shows empty state

(define-test e2e-notification-review-empty ()
  (with-veron-app (s :username "notifuser4" :password "notifpass4")
    (login s "notifuser4" "notifpass4")
    (navigate-to s "NOTI")
    (assert-on-screen s "NOTI")
    (assert-screen-contains s "Datum")))

;;; Notification review screen: shows inbox entries after notification

(define-test e2e-notification-review-with-entries ()
  (with-veron-app (s :username "notifuser5" :password "notifpass5")
    (login s "notifuser5" "notifpass5")
    ;; Manually insert a notification into the inbox
    (veron::with-db
      (let ((uid (pomo:query "SELECT id FROM users WHERE name = 'notifuser5'" :single)))
        (pomo:execute
         "INSERT INTO notification_inbox (user_id, event, title, message)
          VALUES ($1, 'guestbook', 'Test', 'Testnachricht')"
         uid)))
    (navigate-to s "NOTI")
    (assert-on-screen s "NOTI")
    (assert (wait-for-screen-contains s "Testnachricht" :timeout 3)
            () "Inbox should show the test notification")))

;;; Notification review: mark all seen (PF5)

(define-test e2e-notification-review-mark-all-seen ()
  (with-veron-app (s :username "notifuser6" :password "notifpass6")
    (login s "notifuser6" "notifpass6")
    (veron::with-db
      (let ((uid (pomo:query "SELECT id FROM users WHERE name = 'notifuser6'" :single)))
        (pomo:execute
         "INSERT INTO notification_inbox (user_id, event, title, message)
          VALUES ($1, 'login', 'Login', 'Testmeldung')"
         uid)))
    (navigate-to s "NOTI")
    (assert (wait-for-screen-contains s "Testmeldung" :timeout 3)
            () "Should see the notification")
    (press-pf s 5)
    (assert-screen-contains s "Alle als gelesen markiert")
    ;; Verify in DB
    (veron::with-db
      (let ((uid (pomo:query "SELECT id FROM users WHERE name = 'notifuser6'" :single)))
        (let ((unseen (pomo:query
                       "SELECT COUNT(*) FROM notification_inbox WHERE user_id = $1 AND seen = FALSE"
                       uid :single)))
          (assert (zerop unseen) ()
                  "All notifications should be marked seen, got ~D unseen" unseen))))))

;;; Notification review: clear all (PF9 + confirm)

(define-test e2e-notification-review-clear-all ()
  (with-veron-app (s :username "notifuser7" :password "notifpass7")
    (login s "notifuser7" "notifpass7")
    (veron::with-db
      (let ((uid (pomo:query "SELECT id FROM users WHERE name = 'notifuser7'" :single)))
        (pomo:execute
         "INSERT INTO notification_inbox (user_id, event, title, message)
          VALUES ($1, 'logout', 'Logout', 'Abmeldung-Test')"
         uid)))
    (navigate-to s "NOTI")
    (assert (wait-for-screen-contains s "Abmeldung-Test" :timeout 3)
            () "Should see the notification")
    (press-pf s 9)
    (assert-screen-contains s "loeschen")
    (press-pf s 5)
    (assert (wait-for-screen-contains s "geloescht" :timeout 3)
            () "Should see delete confirmation")
    ;; Verify inbox is empty
    (veron::with-db
      (let ((uid (pomo:query "SELECT id FROM users WHERE name = 'notifuser7'" :single)))
        (let ((count (pomo:query
                      "SELECT COUNT(*) FROM notification_inbox WHERE user_id = $1"
                      uid :single)))
          (assert (zerop count) ()
                  "Inbox should be empty, got ~D" count))))))

;;; Originator exclusion: user doesn't get their own guestbook notification

(define-test e2e-notification-originator-exclusion ()
  (with-veron-app (s :username "notifuser8" :password "notifpass8")
    (login s "notifuser8" "notifpass8")
    ;; Enable local guestbook notifications for this user
    (veron::with-db
      (let ((uid (pomo:query "SELECT id FROM users WHERE name = 'notifuser8'" :single)))
        (enable-notifications uid :local t)))
    ;; Write a guestbook entry as this user
    (navigate-to s "GUESTBOOK")
    (assert-on-screen s "GUESTBOOK")
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK-NEW")
    ;; Author field — for logged-in users it's pre-filled, tab to message
    (tab-forward s)
    (type-text s "Testmessage from notifuser8")
    (press-pf s 5)
    ;; Confirm the guestbook entry
    (assert-on-screen s "GUESTBOOK-CONFIRM")
    (press-pf s 5)
    (assert (wait-for-screen-contains s "GUESTBOOK" :timeout 3)
            () "Should return to guestbook")
    ;; Poll until the delivery queue has been processed by checking that
    ;; the guestbook count in the DB reflects the new entry
    (assert (wait-for-screen-match s
              (lambda (full) (declare (ignore full))
                (veron::with-db
                  (plusp (pomo:query "SELECT COUNT(*) FROM guestbook" :single))))
              :timeout 5)
            () "Guestbook entry should exist")
    ;; Check inbox — should be empty (originator excluded)
    (veron::with-db
      (let ((uid (pomo:query "SELECT id FROM users WHERE name = 'notifuser8'" :single)))
        (let ((count (pomo:query
                      "SELECT COUNT(*) FROM notification_inbox WHERE user_id = $1"
                      uid :single)))
          (assert (zerop count) ()
                  "Originator should not receive own notification, got ~D" count))))))

;;; Cross-user notification: user B gets notification when user A writes guestbook

(define-test e2e-notification-cross-user ()
  (with-veron-app (s :username "notifuser9a" :password "notifpass9a")
    ;; Create second user and enable local guestbook notifications
    (create-test-user "notifuser9b" "notifpass9b" :id 99997)
    (veron::with-db
      (let ((uid (pomo:query "SELECT id FROM users WHERE name = 'notifuser9b'" :single)))
        (enable-notifications uid :local t)))
    (login s "notifuser9a" "notifpass9a")
    ;; Write a guestbook entry
    (navigate-to s "GUESTBOOK")
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK-NEW")
    (tab-forward s)
    (type-text s "Cross-user test message")
    (press-pf s 5)
    (assert (wait-for-screen-contains s "GUESTBOOK" :timeout 3)
            () "Should return to guestbook")
    ;; Poll until user B has a notification in inbox
    (assert (wait-for-screen-match s
              (lambda (full) (declare (ignore full))
                (veron::with-db
                  (let ((uid (pomo:query "SELECT id FROM users WHERE name = 'notifuser9b'" :single)))
                    (plusp (pomo:query
                            "SELECT COUNT(*) FROM notification_inbox WHERE user_id = $1"
                            uid :single)))))
              :timeout 5)
            () "User B should have received a notification")))

;;; Message line: confirmation cleared on next interaction

(define-test e2e-message-confirmation-cleared ()
  (with-veron-app (s :username "msguser1" :password "msgpass1")
    (login s "msguser1" "msgpass1")
    ;; Write a guestbook entry — shows confirmation "Eintrag gespeichert"
    (navigate-to s "GUESTBOOK")
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK-NEW")
    (tab-forward s)
    (type-text s "Confirmation clear test")
    (press-pf s 5)
    (assert-on-screen s "GUESTBOOK-CONFIRM")
    (press-pf s 5)
    ;; Should show confirmation on guestbook list
    (assert-on-screen s "GUESTBOOK")
    (assert-message s "Eintrag gespeichert")
    ;; Press Enter — confirmation should be cleared
    (press-enter s)
    (assert-no-message s)))

;;; Message line: error cleared on next interaction

(define-test e2e-message-error-cleared ()
  (with-veron-app (s :username "msguser2" :password "msgpass2")
    (login s "msguser2" "msgpass2")
    ;; Type an invalid command
    (move-cursor s 21 14)
    (erase-eof s)
    (type-text s "xyzzy")
    (press-enter s)
    ;; Error should appear
    (assert-message s "Unbekannter Befehl")
    ;; Press Enter — error should be cleared
    (press-enter s)
    (assert-no-message s)))

;;; Notification mark-as-read on user interaction

(define-test e2e-notification-marked-read-on-dismiss ()
  (with-veron-app (s :username "msguser3" :password "msgpass3")
    (login s "msguser3" "msgpass3")
    ;; Enable local guestbook notifications
    (veron::with-db
      (let ((uid (pomo:query "SELECT id FROM users WHERE name = 'msguser3'" :single)))
        (pomo:execute
         "INSERT INTO notification_settings (user_id, event, ntfy, local)
          VALUES ($1, 'guestbook', FALSE, TRUE)
          ON CONFLICT (user_id, event) DO UPDATE SET ntfy = FALSE, local = TRUE"
         uid)))
    ;; Insert a notification into inbox and trigger delivery
    (veron::with-db
      (let ((uid (pomo:query "SELECT id FROM users WHERE name = 'msguser3'" :single)))
        (veron::add-to-inbox uid :guestbook "Test" "Dismiss-Test")
        ;; Signal pending notification to trigger the update cycle hook
        (veron::deliver-to-session uid)))
    ;; Press Enter a few times to pick up the notification and then dismiss it
    (dotimes (i 5) (press-enter s) (bt:thread-yield))
    ;; The notification should have appeared and been cleared
    ;; Now press Enter once more to trigger message-cleared
    (press-enter s)
    ;; Verify marked as read in DB
    (veron::with-db
      (let ((uid (pomo:query "SELECT id FROM users WHERE name = 'msguser3'" :single)))
        (let ((unseen (pomo:query
                       "SELECT COUNT(*) FROM notification_inbox WHERE user_id = $1 AND seen = FALSE"
                       uid :single)))
          (assert (zerop unseen) ()
                  "Notification should be marked as read, got ~D unseen" unseen))))))

;;; Menu integration: Benachrichtigungen in personal menu, noti navigable

(define-test e2e-notification-menu-item ()
  (with-veron-app (s :username "notifuser10" :password "notifpass10")
    (login s "notifuser10" "notifpass10")
    (select-menu-item s "Persoenlich")
    (assert-on-screen s "PERSONAL")
    (assert-screen-contains s "Benachrichtigungen")
    ;; noti screen is navigable
    (press-pf s 3)
    (navigate-to s "NOTI")
    (assert-on-screen s "NOTI")))
