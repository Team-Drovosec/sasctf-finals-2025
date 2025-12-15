package main

import (
	"chess/internal/repository/chat"
	"chess/internal/repository/game"
	"chess/internal/repository/invite"
	recentgames "chess/internal/repository/recent-games"
	"chess/internal/repository/user"
	"chess/internal/service"
	"context"

	"github.com/jackc/pgx/v5/pgxpool"
)

func main() {
	ctx := context.Background()
	cfg, err := pgxpool.ParseConfig("postgres://root:chessforever@db:5432/chess?sslmode=disable")
	if err != nil {
		panic(err)
	}

	conn, err := pgxpool.NewWithConfig(ctx, cfg)
	if err != nil {
		panic(err)
	}

	u := user.New(conn)
	g := game.New(conn)
	recent := recentgames.New(g)
	i := invite.New(conn, g, recent)
	chats := chat.New(conn)

	srv, err := service.Init(ctx, u, i, g, recent, chats)
	if err != nil {
		panic(err)
	}

	srv.Run()
}
