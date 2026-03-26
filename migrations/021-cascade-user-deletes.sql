ALTER TABLE logins DROP CONSTRAINT logins_user_id_fkey;
ALTER TABLE logins ADD CONSTRAINT logins_user_id_fkey
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE;

ALTER TABLE chat_messages DROP CONSTRAINT chat_messages_user_id_fkey;
ALTER TABLE chat_messages ADD CONSTRAINT chat_messages_user_id_fkey
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE;

ALTER TABLE files DROP CONSTRAINT files_owner_id_fkey;
ALTER TABLE files ADD CONSTRAINT files_owner_id_fkey
  FOREIGN KEY (owner_id) REFERENCES users(id) ON DELETE CASCADE;
