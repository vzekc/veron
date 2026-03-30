-- Per-user, per-event notification delivery preferences
CREATE TABLE notification_settings (
    user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    event VARCHAR(32) NOT NULL,
    ntfy BOOLEAN NOT NULL DEFAULT FALSE,
    local BOOLEAN NOT NULL DEFAULT FALSE,
    PRIMARY KEY (user_id, event)
);

-- Notification inbox for in-app delivery
CREATE TABLE notification_inbox (
    id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    event VARCHAR(32) NOT NULL,
    title VARCHAR(255) NOT NULL,
    message TEXT NOT NULL,
    seen BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_notification_inbox_user ON notification_inbox(user_id, seen, created_at DESC);

-- Per-user ntfy topic and beep preference
ALTER TABLE users ADD COLUMN ntfy_topic VARCHAR(255) NOT NULL DEFAULT '';
ALTER TABLE users ADD COLUMN notification_beep BOOLEAN NOT NULL DEFAULT FALSE;

-- Migrate existing notification_subscriptions data
INSERT INTO notification_settings (user_id, event, ntfy)
SELECT ns.user_id, UNNEST(ns.events), TRUE
FROM notification_subscriptions ns
ON CONFLICT (user_id, event) DO NOTHING;

-- Copy ntfy topics to users table
UPDATE users SET ntfy_topic = sub.topic
FROM (
    SELECT DISTINCT ON (user_id) user_id, topic
    FROM notification_subscriptions
    ORDER BY user_id, created_at DESC
) sub
WHERE users.id = sub.user_id AND sub.topic != '';
