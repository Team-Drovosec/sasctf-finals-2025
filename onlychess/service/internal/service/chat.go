package service

import (
	"chess/internal/repository/chat"
	"log/slog"
	"time"

	"github.com/gin-gonic/gin"
)

func (s *Service) Chat(c *gin.Context) {
	ctx := c.Request.Context()
	gameID := c.Query("id")

	conn, err := upgrader.Upgrade(c.Writer, c.Request, nil)
	if err != nil {
		slog.Error("failed to upgrade connection", "err", err)
		return
	}
	defer conn.Close()

	sendMessage := func(msg chat.Message) error {
		conn.SetWriteDeadline(time.Now().Add(time.Second))

		err := conn.WriteJSON(msg)
		if err != nil {
			conn.Close()
			return err
		}

		return nil
	}

	err = s.chats.AddSubscriber(ctx, gameID, sendMessage)
	if err != nil {
		slog.Error("failed to add subscriber", "err", err)
		return
	}

	for {
		var msg chat.Message
		err = conn.ReadJSON(&msg)
		if err != nil {
			return
		}
		if msg.IsPrivate {
			continue
		}

		s.recentGames.AddGame(ctx, *getUser(c), gameID)
		err = s.chats.Send(ctx, gameID, msg)
		if err != nil {
			slog.Error("chat send error", "err", err)
		}
	}
}
