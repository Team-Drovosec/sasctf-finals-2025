package service

import (
	"chess/internal/model"
	"chess/internal/repository/invite"
	"errors"
	"log/slog"

	"github.com/gin-gonic/gin"
)

func (s *Service) OpenInviteAccept(c *gin.Context) {
	ctx := c.Request.Context()

	req := inviteAcceptReq{}
	if err := c.BindJSON(&req); err != nil {
		slog.Error("bind", err)
		c.AbortWithStatus(418)
		return
	}

	to := getUser(c)
	from, err := model.FromString(req.From)
	if err != nil {
		slog.Error("failed to parse user", err)
		c.AbortWithStatus(418)
		return
	}

	game, err := s.invite.AcceptOpen(ctx, from, to)
	if errors.Is(err, invite.ErrNotFound) {
		c.AbortWithStatus(404)
		return
	}
	if err != nil {
		slog.Error("err", err, "from", req.From, "to", to)
		c.AbortWithStatus(403)
		return
	}

	c.JSON(200, game)
}
