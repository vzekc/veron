ALTER TABLE users ADD COLUMN roles TEXT[] DEFAULT '{}';
UPDATE users SET roles = ARRAY['veron-administrator'] WHERE is_admin = TRUE;
ALTER TABLE users DROP COLUMN is_admin;
