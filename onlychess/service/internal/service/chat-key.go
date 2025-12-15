package service

import (
	"chess/internal/model"
	"log/slog"

	"github.com/gin-gonic/gin"
)

func (s *Service) ChatKey(c *gin.Context) {
	ctx := c.Request.Context()

	user := getUser(c)
	gameID := c.Query("id")

	games, err := s.game.Get(ctx, []string{gameID})
	if err != nil || len(games) != 1 {
		slog.Error("get game", err)
		c.AbortWithStatus(418)
		return
	}
	game := games[0]

	if !model.Equal(game.From, user) && !model.Equal(game.To, user) {
		c.AbortWithStatus(403)
		return
	}

	key, err := s.game.GetKey(ctx, game.ID)
	if err != nil {
		slog.Error("get game key", err)
		c.AbortWithStatus(500)
		return
	}

	c.JSON(200, key)
}
