package service

import (
	"log/slog"
	"net/http"
	"slices"

	"github.com/gin-gonic/gin"
)

func (s *Service) Game(c *gin.Context) {
	s.eng.SetHTMLTemplate(loadTemplates())
	ctx := c.Request.Context()

	user := getUser(c)

	gameID := c.Query("id")
	games, err := s.game.Get(ctx, []string{gameID})
	if err != nil {
		slog.Error("game get", err)
		c.AbortWithStatus(500)
		return
	}
	game := games[0]

	if slices.Contains([]string{game.From.ToString(), game.To.ToString()}, user.ToString()) {
		_ = s.recentGames.AddGame(ctx, *user, gameID)
	}

	c.HTML(http.StatusOK,
		"game.html",
		gin.H{"User": user, "Game": game},
	)
}
