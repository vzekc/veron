-- Invert no_disconnect to disconnect (disconnect=TRUE means connection ends after session)
ALTER TABLE lu_config ADD COLUMN disconnect BOOLEAN NOT NULL DEFAULT FALSE;
UPDATE lu_config SET disconnect = NOT no_disconnect;
ALTER TABLE lu_config DROP COLUMN no_disconnect;

-- Rename default LU from * to DEFAULT
UPDATE lu_config SET name = 'DEFAULT' WHERE name = '*';
