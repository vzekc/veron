ALTER TABLE chat_messages ADD COLUMN message_type VARCHAR(20) NOT NULL DEFAULT 'message';
UPDATE chat_messages SET message_type = 'notification' WHERE message LIKE '--- %';
