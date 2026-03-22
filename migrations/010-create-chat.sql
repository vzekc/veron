CREATE TABLE chat_channels (
    id SERIAL PRIMARY KEY,
    name VARCHAR(50) NOT NULL UNIQUE,
    description VARCHAR(255),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE chat_messages (
    id SERIAL PRIMARY KEY,
    channel_id INTEGER NOT NULL REFERENCES chat_channels(id),
    user_id INTEGER NOT NULL REFERENCES users(id),
    username VARCHAR(100) NOT NULL,
    message TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_chat_messages_channel_time ON chat_messages(channel_id, created_at DESC);
CREATE INDEX idx_chat_messages_channel_id ON chat_messages(channel_id, id DESC);

INSERT INTO chat_channels (name, description) VALUES ('allgemein', 'Allgemeiner Chat')