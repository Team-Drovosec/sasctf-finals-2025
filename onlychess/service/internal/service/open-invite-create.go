package service

import (
	"log/slog"

	"github.com/gin-gonic/gin"
)

func (s *Service) OpenInviteCreate(c *gin.Context) {
	ctx := c.Request.Context()

	from := getUser(c)

	err := s.invite.CreateOpen(ctx, from)
	if err != nil {
		slog.Error("err", err, "from", from)
		c.AbortWithStatus(403)
		return
	}

	c.JSON(200, nil)
}
