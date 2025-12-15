package invite

import (
	"chess/internal/model"
	"chess/internal/repository/game"
	recentgames "chess/internal/repository/recent-games"
	"context"
	"errors"
	"fmt"
	"time"

	"github.com/jackc/pgx/v5/pgxpool"
)

type Invite struct {
	ID        int         `json:"ID"`
	From      *model.User `json:"From"`
	To        *model.User `json:"To"`
	Status    string      `json:"Status"`
	CreatedAt time.Time   `json:"CreatedAt"`
}

type Repository interface {
	Create(ctx context.Context, from, to *model.User) error
	CreateOpen(ctx context.Context, from *model.User) error
	Accept(ctx context.Context, from, to *model.User) (gameID string, err error)
	AcceptOpen(ctx context.Context, from, to *model.User) (gameID string, err error)
	Decline(ctx context.Context, from, to *model.User) error
	ListForUser(ctx context.Context, user *model.User) ([]Invite, error)
	ListOpen(ctx context.Context) ([]Invite, error)
}

var (
	ErrNotFound = errors.New("not found or already used")
)

type repo struct {
	dao *dao

	games       game.Repository
	recentGames recentgames.Repository
}

func New(conn *pgxpool.Pool, games game.Repository, recentGames recentgames.Repository) Repository {
	return &repo{
		dao: &dao{
			conn: conn,
		},
		games:       games,
		recentGames: recentGames,
	}
}

// Accept implements Repository.
func (r *repo) Accept(ctx context.Context, from, to *model.User) (gameID string, err error) {
	err = r.dao.Accept(ctx, from, to)
	if err != nil {
		return "", fmt.Errorf("DB error: %w", err)
	}

	game, err := r.games.Create(ctx, from, to)
	if err != nil {
		return "", fmt.Errorf("create game: %w", err)
	}

	err = r.recentGames.AddGame(ctx, *from, game.ID)
	if err != nil {
		return game.ID, fmt.Errorf("add recent game: %w", err)
	}
	err = r.recentGames.AddGame(ctx, *to, game.ID)
	if err != nil {
		return game.ID, fmt.Errorf("add recent game: %w", err)
	}

	return game.ID, nil
}

// AcceptOpen implements Repository.
func (r *repo) AcceptOpen(ctx context.Context, from, to *model.User) (gameID string, err error) {
	err = r.dao.AcceptOpen(ctx, from, to)
	if err != nil {
		return "", fmt.Errorf("DB error: %w", err)
	}

	game, err := r.games.Create(ctx, from, to)
	if err != nil {
		return "", fmt.Errorf("create game: %w", err)
	}

	err = r.recentGames.AddGame(ctx, *from, game.ID)
	if err != nil {
		return game.ID, fmt.Errorf("add recent game: %w", err)
	}
	err = r.recentGames.AddGame(ctx, *to, game.ID)
	if err != nil {
		return game.ID, fmt.Errorf("add recent game: %w", err)
	}

	return game.ID, nil
}

// Create implements Repository.
func (r *repo) Create(ctx context.Context, from, to *model.User) error {
	return r.dao.Create(ctx, from, to)
}

// CreateOpen implements Repository.
func (r *repo) CreateOpen(ctx context.Context, from *model.User) error {
	return r.dao.CreateOpen(ctx, from)
}

// Decline implements Repository.
func (r *repo) Decline(ctx context.Context, from, to *model.User) error {
	return r.dao.Decline(ctx, from, to)
}

// ListForUser implements Repository.
func (r *repo) ListForUser(ctx context.Context, user *model.User) ([]Invite, error) {
	return r.dao.ListForUser(ctx, user)
}

// ListOpen implements Repository.
func (r *repo) ListOpen(ctx context.Context) ([]Invite, error) {
	return r.dao.ListOpen(ctx)
}
