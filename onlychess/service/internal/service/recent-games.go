package service

import (
	"log/slog"

	"github.com/gin-gonic/gin"
)

func (s *Service) RecentGames(c *gin.Context) {
	ctx := c.Request.Context()

	user := getUser(c)

	games, err := s.recentGames.GetGames(ctx, *user)
	if err != nil {
		slog.Error("error", err)
		c.AbortWithStatus(500)
		return
	}

	c.JSON(200, games)
}
