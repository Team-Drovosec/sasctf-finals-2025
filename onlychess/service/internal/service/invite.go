package service

import (
	"chess/internal/model"
	"log/slog"

	"github.com/gin-contrib/sessions"
	"github.com/gin-gonic/gin"
)

type inviteReq struct {
	To string `json:"to"`
}

func (s *Service) Invite(c *gin.Context) {
	ctx := c.Request.Context()
	slog.Error("coo", "coo", c.Request.Cookies())

	req := inviteReq{}
	if err := c.BindJSON(&req); err != nil {
		slog.Error("bind", err)
		c.AbortWithStatus(418)
		return
	}

	from := getUser(c)
	to, err := model.FromString(req.To)
	if err != nil {
		slog.Error("failed to parse user", err)
		c.AbortWithStatus(418)
		return
	}

	session := sessions.Default(c)
	slog.Error("saving", "sess", session.ID(), "user", getUser(c))

	err = s.invite.Create(ctx, from, to)
	if err != nil {
		slog.Error("err", err)
		c.AbortWithStatus(403)
		return
	}

	c.JSON(200, nil)
}
