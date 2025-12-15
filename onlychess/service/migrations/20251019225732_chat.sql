-- +goose Up
CREATE TABLE chat (
    id SERIAL PRIMARY KEY,
    game_id TEXT NOT NULL,
    message   TEXT NOT NULL,

    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- +goose Down
DROP TABLE chat;
