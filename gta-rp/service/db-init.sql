CREATE DATABASE gtarp OWNER gtarp_admin;

\connect gtarp

CREATE COLLATION IF NOT EXISTS und_ci
    (provider = icu, locale = 'und@colStrength=secondary', deterministic = false);

CREATE TABLE IF NOT EXISTS users (
    id TEXT PRIMARY KEY,
    username TEXT COLLATE und_ci UNIQUE NOT NULL,
    otp_secret TEXT NOT NULL,
    registered_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS sessions (
    id TEXT PRIMARY KEY,
    access_token TEXT UNIQUE NOT NULL,
    client_token TEXT NOT NULL,
    user_id TEXT NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    expires_at TIMESTAMPTZ NOT NULL
);

CREATE TABLE IF NOT EXISTS mine_owners (
    owner_name TEXT COLLATE und_ci PRIMARY KEY REFERENCES users (username) ON DELETE CASCADE,
    salary_block TEXT NOT NULL DEFAULT 'GOLD_INGOT',
    xp_per_block INTEGER NOT NULL DEFAULT 10,
    spawn_x DOUBLE PRECISION NOT NULL DEFAULT 0,
    spawn_y DOUBLE PRECISION NOT NULL DEFAULT 0,
    spawn_z DOUBLE PRECISION NOT NULL DEFAULT 0,
    level0_message TEXT NOT NULL DEFAULT 'Welcome to the mine!',
    level1_message TEXT NOT NULL DEFAULT 'Keep mining, you are getting better.',
    level2_message TEXT NOT NULL DEFAULT 'Solid progress, miner.',
    level3_message TEXT NOT NULL DEFAULT 'Veteran of the shafts.',
    level4_message TEXT NOT NULL DEFAULT 'Almost a legend underground.',
    level5_message TEXT NOT NULL DEFAULT 'Master of the mine!'
);

CREATE TABLE IF NOT EXISTS worker_employments (
    worker_name TEXT COLLATE und_ci NOT NULL REFERENCES users (username) ON DELETE CASCADE,
    employer_name TEXT COLLATE und_ci NOT NULL REFERENCES mine_owners (owner_name) ON DELETE CASCADE,
    experience INTEGER NOT NULL DEFAULT 0,
    level INTEGER NOT NULL DEFAULT 0,
    is_active BOOLEAN NOT NULL DEFAULT FALSE,
    PRIMARY KEY (worker_name, employer_name)
);
