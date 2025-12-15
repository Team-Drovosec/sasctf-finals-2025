-- +goose Up
CREATE TABLE invites (
    id SERIAL PRIMARY KEY,
    "from" TEXT NOT NULL,
    "to"   TEXT,
    status    TEXT NOT NULL DEFAULT 'pending', -- pending, accepted, declined
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- +goose Down
DROP TABLE invites;
