package service

import (
	"log/slog"

	"github.com/gin-gonic/gin"
)

func (s *Service) OpenInvites(c *gin.Context) {
	ctx := c.Request.Context()

	invites, err := s.invite.ListOpen(ctx)
	if err != nil {
		slog.Error("error", err)
		c.AbortWithStatus(500)
		return
	}

	c.JSON(200, invites)
}
