CREATE TABLE lu_config (
    name VARCHAR(80) PRIMARY KEY,
    description VARCHAR(80) NOT NULL DEFAULT '',
    no_disconnect BOOLEAN NOT NULL DEFAULT TRUE,
    single_instance BOOLEAN NOT NULL DEFAULT FALSE,
    allowed_ips TEXT NOT NULL DEFAULT ''
);

INSERT INTO lu_config (name, description, no_disconnect, allowed_ips)
VALUES ('*', 'Standard LU', TRUE, '');
