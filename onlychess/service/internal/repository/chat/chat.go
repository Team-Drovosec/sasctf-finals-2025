package chat

import (
	"context"
	"encoding/json"
	"errors"
	"sync"
	"time"

	"github.com/jackc/pgx/v5/pgxpool"
)

type Message struct {
	User      string `json:"user"`
	IsPrivate bool   `json:"is_private"`
	IV        []int  `json:"iv"`
	Content   string `json"content"`
}

type Repository interface {
	Send(ctx context.Context, gameID string, message Message) error
	AddSubscriber(ctx context.Context, gameID string, f func(msg Message) error) error
}

type repo struct {
	conn *pgxpool.Pool

	subscribers   map[string][]func(msg Message) error // gameID -> subscribers
	subscribersMx *sync.Mutex
}

func New(conn *pgxpool.Pool) Repository {
	return &repo{
		conn:          conn,
		subscribers:   map[string][]func(msg Message) error{},
		subscribersMx: &sync.Mutex{},
	}
}

func (r *repo) AddSubscriber(ctx context.Context, gameID string, f func(msg Message) error) error {
	rows, err := r.conn.Query(ctx,
		`SELECT message FROM chat WHERE game_id=$1 ORDER BY id ASC`,
		gameID,
	)
	if err != nil {
		return err
	}
	defer rows.Close()

	var oldMessages []Message
	for rows.Next() {
		var msgRaw []byte

		if err := rows.Scan(&msgRaw); err != nil {
			return err
		}

		var msg Message
		err := json.Unmarshal(msgRaw, &msg)
		if err != nil {
			return err
		}

		oldMessages = append(oldMessages, msg)
	}
	if rows.Err() != nil {
		return rows.Err()
	}

	for _, msg := range oldMessages {
		err = f(msg)
		if err != nil {
			return err
		}
	}

	r.subscribersMx.Lock()
	defer r.subscribersMx.Unlock()

	subs, ok := r.subscribers[gameID]
	if !ok {
		subs = make([]func(msg Message) error, 0)
	}
	subs = append(subs, f)
	r.subscribers[gameID] = subs

	return nil
}

func (r *repo) Send(ctx context.Context, gameID string, message Message) error {
	msg, err := json.Marshal(message)
	if err != nil {
		return err
	}

	_, err = r.conn.Exec(ctx,
		`INSERT INTO chat (game_id, message) VALUES ($1, $2)`,
		gameID, msg,
	)
	if err != nil {
		return err
	}

	go func() {
		r.subscribersMx.Lock()
		defer r.subscribersMx.Unlock()

		validSubs := make([]func(msg Message) error, 0, len(r.subscribers[gameID]))
		for _, sub := range r.subscribers[gameID] {
			ctx, cancel := context.WithTimeout(context.Background(), 200*time.Millisecond)
			go func() {
				sub(message)
				cancel()
			}()

			<-ctx.Done()
			if errors.Is(ctx.Err(), context.DeadlineExceeded) {
				continue // dead subscriber
			}

			validSubs = append(validSubs, sub)
		}
		r.subscribers[gameID] = validSubs
	}()

	return nil
}
