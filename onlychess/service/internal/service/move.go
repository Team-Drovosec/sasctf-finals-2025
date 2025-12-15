package service

import (
	"log/slog"

	"github.com/gin-gonic/gin"
)

type moveReq struct {
	GameID   string `json:"game"`
	MoveFrom string `json:"from"`
	MoveTo   string `json:"to"`
}

func (s *Service) Move(c *gin.Context) {
	ctx := c.Request.Context()

	req := moveReq{}
	if err := c.BindJSON(&req); err != nil {
		slog.Error("bind", err)
		c.AbortWithStatus(418)
		return
	}
	user := getUser(c)

	err := s.game.Move(ctx, req.GameID, user, req.MoveFrom, req.MoveTo)
	if err != nil {
		slog.Error("err", err)
		c.AbortWithStatus(403)
		return
	}

	c.JSON(200, nil)
}
