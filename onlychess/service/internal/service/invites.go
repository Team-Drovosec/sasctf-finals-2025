package service

import (
	"log/slog"

	"github.com/gin-gonic/gin"
)

func (s *Service) Invites(c *gin.Context) {
	ctx := c.Request.Context()

	user := getUser(c)

	invites, err := s.invite.ListForUser(ctx, user)
	if err != nil {
		slog.Error("error", err)
		c.AbortWithStatus(500)
		return
	}

	c.JSON(200, invites)
}
