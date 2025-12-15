package service

import (
	"log/slog"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/gorilla/websocket"
)

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

type moveMessage struct {
	From string `json:"from"`
	To   string `json:"to"`
}

func (s *Service) NextMoves(c *gin.Context) {
	gameID := c.Query("id")

	conn, err := upgrader.Upgrade(c.Writer, c.Request, nil)
	if err != nil {
		slog.Error("failed to upgrade connection", "err", err)
		return
	}
	defer conn.Close()

	movesCh, chID := s.game.GetMovesChan(gameID)

	defer conn.Close()
	defer s.game.CloseChan(gameID, chID)

	defer func() {
		slog.Info("conn close")
	}()
	for move := range movesCh {
		conn.SetWriteDeadline(time.Now().Add(time.Second))

		err = conn.WriteJSON(moveMessage{
			From: move.S1().String(),
			To:   move.S2().String(),
		})
		if err != nil {
			return
		}
	}
}
