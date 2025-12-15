package service

import (
	"chess/internal/model"
	"chess/internal/util"
	"log/slog"

	"github.com/gin-contrib/sessions"
	"github.com/gin-gonic/gin"
)

type loginReq struct {
	Username string `json:"username"`
	Password string `json:"password"`
}

func (s *Service) Login(c *gin.Context) {
	ctx := c.Request.Context()

	anonUser := getUser(c)

	req := loginReq{}
	if err := c.BindJSON(&req); err != nil {
		slog.Error("bind", err)
		c.AbortWithStatus(418)
		return
	}

	needPassword, err := s.user.GetPassword(ctx, req.Username)
	if err != nil {
		slog.Error("getuser", err)
		c.AbortWithStatus(523)
		return
	}

	if util.HashSHA256(req.Password) != needPassword {
		slog.Error("wrong passwd")
		c.AbortWithStatus(333)

		return
	}

	session := sessions.Default(c)
	session.Set("username", (&model.User{
		Username: req.Username,
	}).ToString())
	session.Save()

	newUser := getUser(c)

	recentGames, _ := s.recentGames.GetGameIDs(ctx, *anonUser)
	gamesPlayers := s.game.GetPlayers(ctx, recentGames)
	for game, gamePlayers := range gamesPlayers {
		for i := range len(gamePlayers) {
			if model.Equal(&gamePlayers[i], anonUser) {
				err = s.game.Deanonimize(ctx, game, *newUser, i%2 == 0)
				if err != nil {
					slog.Error("error deanon game", "err", err)
				}
				err = s.recentGames.AddGame(ctx, *newUser, game)
				if err != nil {
					slog.Error("error add deanon game", "err", err)
				}
				break
			}
		}
	}

	c.JSON(200, nil)
}
