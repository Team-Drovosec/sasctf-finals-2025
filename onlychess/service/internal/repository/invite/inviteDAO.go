package invite

import (
	"chess/internal/model"
	"context"
	"errors"

	"github.com/jackc/pgx/v5/pgxpool"
)

type dao struct {
	conn *pgxpool.Pool
}

func (r *dao) Create(ctx context.Context, from, to *model.User) error {
	_, err := r.conn.Exec(ctx,
		`INSERT INTO invites ("from", "to") VALUES ($1, $2)`,
		from.ToString(), to.ToString(),
	)
	return err
}

func (r *dao) CreateOpen(ctx context.Context, from *model.User) error {
	_, err := r.conn.Exec(ctx,
		`INSERT INTO invites ("from") VALUES ($1)`,
		from.ToString(),
	)
	return err
}

func (r *dao) Accept(ctx context.Context, from, to *model.User) error {
	ct, err := r.conn.Exec(ctx,
		`UPDATE invites SET status='accepted' WHERE "from"=$1 AND "to"=$2 AND status='pending'`,
		from.ToString(), to.ToString(),
	)
	if err != nil {
		return err
	}
	if ct.RowsAffected() == 0 {
		return errors.New("invite not found or already handled")
	}
	return nil
}

func (r *dao) AcceptOpen(ctx context.Context, from, to *model.User) error {
	ct, err := r.conn.Exec(ctx,
		`UPDATE invites SET status='accepted', "to"=$2 WHERE "from"=$1 AND "to" is null AND status='pending'`,
		from.ToString(), to.ToString(),
	)
	if err != nil {
		return err
	}
	if ct.RowsAffected() == 0 {
		return ErrNotFound
	}
	return nil
}

func (r *dao) Decline(ctx context.Context, from, to *model.User) error {
	ct, err := r.conn.Exec(ctx,
		`UPDATE invites SET status='decline' WHERE "from"=$1 AND "to"=$2 AND status='pending'`,
		from.ToString(), to.ToString(),
	)
	if err != nil {
		return err
	}
	if ct.RowsAffected() == 0 {
		return errors.New("invite not found or already handled")
	}
	return nil
}

func (r *dao) ListForUser(ctx context.Context, user *model.User) ([]Invite, error) {
	rows, err := r.conn.Query(ctx,
		`SELECT id, "from", "to", status, created_at
         FROM invites
         WHERE "to"=$1 AND status='pending'
         ORDER BY created_at DESC
		 LIMIT 100`,
		user.ToString(),
	)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var result []Invite
	for rows.Next() {
		var inv Invite
		var from, to string

		if err := rows.Scan(&inv.ID, &from, &to, &inv.Status, &inv.CreatedAt); err != nil {
			return nil, err
		}

		inv.From, err = model.FromString(from)
		if err != nil {
			return nil, err
		}
		inv.To, err = model.FromString(to)
		if err != nil {
			return nil, err
		}

		result = append(result, inv)
	}
	if rows.Err() != nil {
		return nil, rows.Err()
	}

	return result, nil
}

func (r *dao) ListOpen(ctx context.Context) ([]Invite, error) {
	rows, err := r.conn.Query(ctx,
		`SELECT id, "from", status, created_at
         FROM invites
         WHERE "to" is null AND status='pending'
         ORDER BY created_at DESC
		 LIMIT 100`,
	)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var result []Invite
	for rows.Next() {
		var inv Invite
		var from string
		if err := rows.Scan(&inv.ID, &from, &inv.Status, &inv.CreatedAt); err != nil {
			return nil, err
		}

		inv.From, err = model.FromString(from)
		if err != nil {
			return nil, err
		}

		result = append(result, inv)
	}
	if rows.Err() != nil {
		return nil, rows.Err()
	}

	return result, nil
}
