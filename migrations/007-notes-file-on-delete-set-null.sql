ALTER TABLE users DROP CONSTRAINT users_notes_file_id_fkey;
ALTER TABLE users ADD CONSTRAINT users_notes_file_id_fkey FOREIGN KEY (notes_file_id) REFERENCES files(id) ON DELETE SET NULL