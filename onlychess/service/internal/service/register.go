package service

import (
	"chess/internal/model"
	"chess/internal/util"
	"log/slog"

	"github.com/gin-gonic/gin"
)

type registerReq struct {
	Username string `json:"username"`
	Password string `json:"password"`
}

func (s *Service) Register(c *gin.Context) {
	ctx := c.Request.Context()

	req := registerReq{}
	if err := c.BindJSON(&req); err != nil {
		slog.Error("parse", "err", err)
		c.AbortWithStatus(532)
		return
	}

	if !model.ValidateUsername(req.Username) {
		c.AbortWithStatus(403)
		return
	}

	hashPasswd := util.HashSHA256(req.Password)
	err := s.user.Save(ctx, req.Username, hashPasswd)
	if err != nil {
		slog.Error("save", "err", err)
		c.AbortWithStatus(514)
		return
	}

	c.JSON(200, nil)
}
