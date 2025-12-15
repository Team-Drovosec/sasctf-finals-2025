package service

import (
	"chess/internal/model"
	"log/slog"

	"github.com/gin-gonic/gin"
)

type inviteAcceptReq struct {
	From string `json:"from"`
}

func (s *Service) InviteAccept(c *gin.Context) {
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

	game, err := s.invite.Accept(ctx, from, to)
	if err != nil {
		slog.Error("err", err, "from", from, "to", to)
		c.AbortWithStatus(403)
		return
	}

	c.JSON(200, game)
}
