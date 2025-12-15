package service

import (
	"chess/internal/model"
	"chess/internal/repository/chat"
	"chess/internal/repository/game"
	"chess/internal/repository/invite"
	recentgames "chess/internal/repository/recent-games"
	"chess/internal/repository/user"
	"chess/internal/util"
	"context"
	"crypto/rand"
	"encoding/hex"
	"html/template"
	"log/slog"
	"net/http"
	"os"

	"github.com/gin-contrib/sessions"
	"github.com/gin-contrib/sessions/cookie"
	"github.com/gin-gonic/gin"
)

type Service struct {
	eng *gin.Engine

	user        user.Repository
	invite      invite.Repository
	game        game.Repository
	recentGames recentgames.Repository
	chats       chat.Repository

	encryptKey []int
}

func Init(
	ctx context.Context,
	user user.Repository,
	invite invite.Repository,
	game game.Repository,
	recentGames recentgames.Repository,
	chats chat.Repository,
) (Service, error) {
	return Service{
		eng:         gin.Default(),
		user:        user,
		invite:      invite,
		game:        game,
		recentGames: recentGames,
		chats:       chats,
	}, nil
}

func (s *Service) Run() {
	s.routes()
	s.eng.Run(":4000")
}

func CORSMiddleware() gin.HandlerFunc {
	return func(c *gin.Context) {
		// c.Writer.Header().Set("Access-Control-Allow-Origin", "http://localhost:37755")
		c.Writer.Header().Set("Access-Control-Allow-Origin", "*")
		c.Writer.Header().Set("Access-Control-Allow-Credentials", "true")
		c.Writer.Header().Set("Access-Control-Allow-Headers", "Content-Type, Content-Length, Accept-Encoding, X-CSRF-Token, Authorization, accept, origin, Cache-Control, X-Requested-With")
		c.Writer.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS, GET, PUT")

		if c.Request.Method == "OPTIONS" {
			c.AbortWithStatus(204)
			return
		}

		c.Next()
	}
}

func ginBodyLogMiddleware(c *gin.Context) {
	for _, err := range c.Errors {
		if err == nil {
			slog.Error("nil error")
			continue
		}
		slog.Error("error", "err", err.Error())
	}
}

func loadTemplates() *template.Template {
	return template.Must(template.New("").ParseGlob("templates/*.*html"))
}

func (s *Service) routes() {
	eng := gin.Default()
	eng.Use(CORSMiddleware())
	eng.Use(ginBodyLogMiddleware)

	store := cookie.NewStore([]byte(chooseCookieSecret()))
	store.Options(sessions.Options{
		MaxAge:   3600,  // 1 hour
		HttpOnly: false, // Prevents JavaScript access
		Secure:   false, // Set to true for production
	})
	eng.Use(sessions.Sessions("session", store))

	eng.GET("/", func(c *gin.Context) {
		eng.SetHTMLTemplate(loadTemplates()) // todo move up
		user := getUser(c)
		c.HTML(http.StatusOK, "index.html", gin.H{"User": user})
	})

	eng.POST("/login", s.Login)
	eng.POST("/register", s.Register)

	eng.POST("/invite", s.Invite)
	eng.POST("/invite-accept", s.InviteAccept)
	eng.POST("/invite-decline", s.InviteDecline)
	eng.GET("/invites", s.Invites)
	eng.GET("/open-invites", s.OpenInvites)
	eng.POST("/open-invite-create", s.OpenInviteCreate)
	eng.POST("/open-invite-accept", s.OpenInviteAccept)

	eng.GET("/game", s.Game)
	eng.POST("/move", s.Move)
	eng.GET("/next-moves", s.NextMoves)

	eng.GET("/recent-games", s.RecentGames)

	eng.GET("/chat", s.Chat)
	eng.POST("/private-chat", s.PrivateChat)
	eng.GET("/chat-key", s.ChatKey)

	s.eng = eng
}

// In this app any user is authorized
func getUser(c *gin.Context) *model.User {
	session := sessions.Default(c)
	uname := session.Get("username")

	userStr, ok := uname.(string)
	if userStr == "" || !ok {
		anonID, err := util.RandInt32()
		if err != nil {
			slog.Error("cant auth")
			c.AbortWithStatus(403)
			return nil
		}

		userStr = (&model.User{
			AnonymousID: anonID,
		}).ToString()

		session.Set("username", userStr)
		session.Save()
	}

	user, err := model.FromString(userStr)
	if err != nil {
		slog.Error("invalid session")
		c.AbortWithStatus(403)
		session.Set("username", nil)
		session.Save()
		return nil
	}

	return user
}

func chooseCookieSecret() string {
	const secretFile = "cookie_secret"

	if data, err := os.ReadFile(secretFile); err == nil && len(data) > 0 {
		return string(data)
	}

	secret := make([]byte, 32)
	if _, err := rand.Read(secret); err != nil {
		panic("failed to generate random secret: " + err.Error())
	}

	secretHex := hex.EncodeToString(secret)

	if err := os.WriteFile(secretFile, []byte(secretHex), 0600); err != nil {
		panic("failed to write secret file: " + err.Error())
	}

	return secretHex
}
