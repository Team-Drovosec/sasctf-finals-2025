package recentgames

import (
	"chess/internal/model"
	"chess/internal/repository/game"
	"context"
	"fmt"
	"slices"
	"sync"
	"time"

	"github.com/samber/lo"
)

type Repository interface {
	AddGame(ctx context.Context, user model.User, gameID string) error
	GetGames(ctx context.Context, user model.User) ([]game.Game, error)
	GetGameIDs(ctx context.Context, user model.User) ([]string, error)
}

// Recent games are stored in a ring buffer of size bufferSize. gameNextI shows the next item that will be overwritten
var bufferSize = 10

// After this time of inactivity the user games should be deleted
var userDeletionTimeout = 1 * time.Hour

type repo struct {
	mp          map[model.User][]string // user -> gameID
	nextI       map[model.User]int      // least recent game
	deleteTimer map[model.User]*time.Timer

	lock *sync.RWMutex

	games game.Repository
}

// AddGame implements Repository.
func (r *repo) AddGame(ctx context.Context, user model.User, gameID string) error {
	games, err := r.GetGameIDs(ctx, user)
	if err != nil {
		return fmt.Errorf("get games: %w", err)
	}
	if slices.Contains(games, gameID) {
		return nil
	}

	r.lock.Lock()
	defer r.lock.Unlock()

	userGames, ok := r.mp[user]
	if !ok {
		r.nextI[user] = 0
		userGames = make([]string, bufferSize)
	}

	userGames[r.nextI[user]] = gameID
	r.nextI[user] = (r.nextI[user] + 1) % bufferSize
	r.mp[user] = userGames

	timer, ok := r.deleteTimer[user]
	if !ok {
		r.deleteTimer[user] = time.AfterFunc(userDeletionTimeout, func() {
			r.deleteGames(context.Background(), user)
		})
	} else {
		timer.Reset(userDeletionTimeout)
	}

	return nil
}

func (r *repo) GetGameIDs(_ context.Context, user model.User) ([]string, error) {
	r.lock.RLock()
	defer r.lock.RUnlock()

	games, ok := r.mp[user]
	if !ok {
		return nil, nil
	}
	i := r.nextI[user]

	games = append(games[i:], games[:i]...)
	slices.Reverse(games)

	games = lo.Filter(games, func(g string, _ int) bool {
		return g != ""
	})

	return games, nil
}

// GetGames returns recent games
func (r *repo) GetGames(ctx context.Context, user model.User) ([]game.Game, error) {
	games, err := r.GetGameIDs(ctx, user)
	if err != nil {
		return nil, fmt.Errorf("get game IDs: %w", err)
	}

	gamesFull, err := r.games.Get(ctx, games)
	if err != nil {
		return nil, fmt.Errorf("get game info: %w", err)
	}

	return gamesFull, nil
}

func (r *repo) deleteGames(_ context.Context, user model.User) error {
	r.lock.Lock()
	defer r.lock.Unlock()

	delete(r.mp, user)
	delete(r.deleteTimer, user)
	delete(r.nextI, user)

	return nil
}

func New(games game.Repository) Repository {
	return &repo{
		mp:          map[model.User][]string{},
		nextI:       map[model.User]int{},
		deleteTimer: map[model.User]*time.Timer{},
		lock:        &sync.RWMutex{},
		games:       games,
	}
}
