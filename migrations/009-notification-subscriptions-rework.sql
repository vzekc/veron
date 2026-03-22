ALTER TABLE notification_subscriptions ADD COLUMN topic VARCHAR(255);
UPDATE notification_subscriptions SET topic = endpoint;
ALTER TABLE notification_subscriptions ALTER COLUMN topic SET NOT NULL;
ALTER TABLE notification_subscriptions DROP COLUMN endpoint;
ALTER TABLE notification_subscriptions DROP COLUMN events;
ALTER TABLE notification_subscriptions ADD COLUMN events TEXT[] NOT NULL DEFAULT '{}'