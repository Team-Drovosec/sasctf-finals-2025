package service

import (
	"chess/internal/lfsr"
	"chess/internal/repository/chat"
	"chess/internal/util"
	"encoding/base64"
	"log/slog"

	"github.com/gin-gonic/gin"
)

type chatMessage struct {
	GameID  string `json:"game"`
	Message string `json:"message"`
}

func (s *Service) PrivateChat(c *gin.Context) {
	ctx := c.Request.Context()

	req := chatMessage{}
	if err := c.BindJSON(&req); err != nil {
		slog.Error("bind", err)
		c.AbortWithStatus(418)
		return
	}
	user := getUser(c)
	games, err := s.game.Get(ctx, []string{req.GameID})
	if err != nil || len(games) != 1 {
		slog.Error("get game", err)
		c.AbortWithStatus(418)
		return
	}
	game := games[0]

	key, err := s.game.GetKey(ctx, game.ID)
	if err != nil {
		slog.Error("get game key", err)
		c.AbortWithStatus(500)
		return
	}

	cip, err := lfsr.New(key)
	if err != nil {
		slog.Error("crypto err create", err)
		c.AbortWithStatus(500)
		return
	}

	iv, err := lfsr.KeyGen()
	if err != nil {
		slog.Error("crypto err create", err)
		c.AbortWithStatus(500)
		return
	}

	encrypted, err := cip.Crypt(util.StrToBitArray(req.Message), iv)
	if err != nil {
		slog.Error("crypto err create", err)
		c.AbortWithStatus(500)
		return
	}
	encryptedBytes, err := util.BitArrayToStr(encrypted)
	if err != nil {
		slog.Error("crypto err convert", err)
		c.AbortWithStatus(500)
		return
	}

	err = s.chats.Send(
		ctx,
		game.ID,
		chat.Message{
			User:      user.ToString(),
			IsPrivate: true,
			IV:        iv,
			Content:   base64.StdEncoding.EncodeToString([]byte(encryptedBytes)),
		},
	)
	if err != nil {
		slog.Error("err", err)
		c.AbortWithStatus(403)
		return
	}

	c.JSON(200, nil)
}
