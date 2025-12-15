package user

import (
	"context"
	"errors"

	"github.com/jackc/pgx/v5"
	"github.com/jackc/pgx/v5/pgxpool"
)

type Repository interface {
	Save(ctx context.Context, user, password string) error
	GetPassword(ctx context.Context, user string) (string, error)
}

type user struct {
	pg *pgxpool.Pool
}

func New(pg *pgxpool.Pool) Repository {
	return &user{pg: pg}
}

func (u *user) Save(ctx context.Context, username, password string) error {
	_, err := u.pg.Exec(ctx,
		`INSERT INTO users (username, password) 
         VALUES ($1, $2)
         ON CONFLICT (username) DO NOTHING`,
		username, password,
	)
	return err
}

func (u *user) GetPassword(ctx context.Context, username string) (string, error) {
	var password string

	err := u.pg.QueryRow(ctx,
		`SELECT password FROM users WHERE username=$1`,
		username,
	).Scan(&password)

	if err != nil {
		if err == pgx.ErrNoRows {
			return "", errors.New("user not found")
		}
		return "", err
	}
	return password, nil
}
