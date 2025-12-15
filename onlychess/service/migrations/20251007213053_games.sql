-- +goose Up
CREATE TABLE game (
    id TEXT PRIMARY KEY,
    "from" TEXT NOT NULL,
    "to"   TEXT NOT NULL,

    status TEXT NOT NULL DEFAULT 'pending', -- pending, from_win, to_win
    is_white_move BOOLEAN NOT NULL,
    "key" TEXT NOT NULL,

    chess_fen_state TEXT NOT NULL DEFAULT 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1',

    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- +goose Down
DROP TABLE game;
