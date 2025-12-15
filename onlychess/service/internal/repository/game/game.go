package game

import (
	"chess/internal/lfsr"
	"chess/internal/model"
	"chess/internal/util"
	"context"
	"crypto/rand"
	"errors"
	"fmt"
	"sync"
	"time"

	"github.com/corentings/chess/v2"
	"github.com/jackc/pgx/v5/pgxpool"
)

type Game struct {
	ID          string      `json:"ID"`
	From        *model.User `json:"From"`
	To          *model.User `json:"To"`
	Status      string      `json:"Status"`
	IsWhiteMove bool        `json:"IsWhiteMove"`
	FEN         string      `json:"FEN"`

	CreatedAt time.Time `json:"CreatedAt"`
}

type Repository interface {
	Create(ctx context.Context, from, to *model.User) (Game, error)
	Get(ctx context.Context, ID []string) ([]Game, error)
	GetKey(ctx context.Context, ID string) ([]int, error)
	GetPlayers(ctx context.Context, ID []string) map[string][]model.User
	Deanonimize(ctx context.Context, ID string, user model.User, isWhite bool) error

	Move(ctx context.Context, ID string, user *model.User, moveFrom, moveTo string) error
	GetMovesChan(gameID string) (ch chan *chess.Move, chanID string)
	CloseChan(gameID, chanID string)
}

type repo struct {
	conn *pgxpool.Pool

	moveChans   map[string]map[string]chan *chess.Move
	moveChansMx *sync.Mutex
}

func New(conn *pgxpool.Pool) Repository {
	return &repo{
		conn:        conn,
		moveChans:   map[string]map[string]chan *chess.Move{},
		moveChansMx: &sync.Mutex{},
	}
}

func (r *repo) CloseChan(gameID string, chanID string) {
	r.moveChansMx.Lock()
	defer r.moveChansMx.Unlock()

	chans, ok := r.moveChans[gameID]
	if !ok {
		return
	}

	close(chans[chanID])
	delete(chans, chanID)
}

func (r *repo) GetMovesChan(gameID string) (ch chan *chess.Move, chanID string) {
	r.moveChansMx.Lock()
	defer r.moveChansMx.Unlock()

	chans, ok := r.moveChans[gameID]
	if !ok {
		chans = make(map[string]chan *chess.Move, 1)
	}

	ch = make(chan *chess.Move)
	chID := rand.Text()
	chans[chID] = ch
	r.moveChans[gameID] = chans

	return ch, chID
}

// Get returns multiple games by their IDs
func (r *repo) Get(ctx context.Context, IDs []string) ([]Game, error) {
	rows, err := r.conn.Query(ctx,
		`SELECT id, "from", "to", status, is_white_move, created_at, chess_fen_state FROM game WHERE id = ANY($1)`,
		IDs,
	)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	games := make(map[string]Game)
	for rows.Next() {
		var (
			id, fromStr, toStr, status, fen string
			isWhiteMove                     bool
			createdAt                       time.Time
		)
		if err := rows.Scan(&id, &fromStr, &toStr, &status, &isWhiteMove, &createdAt, &fen); err != nil {
			return nil, err
		}

		fromUser, err := model.FromString(fromStr)
		if err != nil {
			return nil, fmt.Errorf("invalid 'from' user: %w", err)
		}
		toUser, err := model.FromString(toStr)
		if err != nil {
			return nil, fmt.Errorf("invalid 'to' user: %w", err)
		}

		games[id] = Game{
			ID:          id,
			From:        fromUser,
			To:          toUser,
			Status:      status,
			IsWhiteMove: isWhiteMove,
			CreatedAt:   createdAt,
			FEN:         fen,
		}
	}
	if rows.Err() != nil {
		return nil, rows.Err()
	}

	// ensuring the order of the request
	gamesOrdered := make([]Game, len(IDs))
	for i, gameID := range IDs {
		gamesOrdered[i] = games[gameID]
	}

	return gamesOrdered, nil
}

func (r *repo) Create(ctx context.Context, fromU, toU *model.User) (Game, error) {
	id := rand.Text()

	key, err := lfsr.KeyGen()
	if err != nil {
		return Game{}, err
	}
	keyStr := string(util.Map(key, util.BitToByte))

	from, to := fromU.ToString(), toU.ToString()
	if from == to {
		return Game{}, errors.New("cat play yourself")
	}

	_, err = r.conn.Exec(ctx,
		`INSERT INTO game (id, "from", "to", status, is_white_move, "key") VALUES ($1, $2, $3, 'started', true, $4)`,
		id, from, to, keyStr,
	)
	return Game{
		ID:          id,
		From:        fromU,
		To:          toU,
		Status:      "started",
		IsWhiteMove: true,
		CreatedAt:   time.Time{},
	}, err
}

func (r *repo) GetKey(ctx context.Context, ID string) ([]int, error) {
	var keyStr string
	err := r.conn.QueryRow(ctx,
		`SELECT "key" FROM game WHERE id = $1`,
		ID,
	).Scan(&keyStr)
	if err != nil {
		return nil, fmt.Errorf("get key: %w", err)
	}

	return util.Map([]byte(keyStr), util.ByteToBit), nil
}

var (
	ErrNotAValidMove = errors.New("not a valid move")
	ErrNotYourMove   = errors.New("not your move")
)

// Move implements Repository.
func (r *repo) Move(ctx context.Context, ID string, user *model.User, moveFrom string, moveTo string) error {
	games, err := r.Get(ctx, []string{ID})
	if err != nil {
		return fmt.Errorf("get game: %w", err)
	}
	if len(games) != 1 {
		return fmt.Errorf("game not found")
	}
	game := games[0]

	if (game.IsWhiteMove && !model.Equal(game.From, user)) || (!game.IsWhiteMove && !model.Equal(game.To, user)) {
		return ErrNotYourMove
	}

	fen, err := chess.FEN(game.FEN)
	if err != nil {
		return fmt.Errorf("game decoding: %w", err)
	}
	chessGame := chess.NewGame(fen)

	var move *chess.Move = nil
	for _, validMove := range chessGame.ValidMoves() {
		if validMove.S1().String() == moveFrom && validMove.S2().String() == moveTo {
			move = &validMove
			break
		}
	}
	if move == nil {
		return ErrNotAValidMove
	}

	err = chessGame.Move(move, nil)
	if err != nil {
		return err
	}

	// todo check if game is finished

	newFEN := chessGame.FEN()
	isWhiteMove := !game.IsWhiteMove

	_, err = r.conn.Exec(ctx,
		`UPDATE game SET chess_fen_state=$2, is_white_move=$3 WHERE id=$1`,
		game.ID, newFEN, isWhiteMove,
	)
	if err != nil {
		return err
	}
	go func() {
		r.moveChansMx.Lock()
		moveChans := r.moveChans[game.ID]
		r.moveChansMx.Unlock()

		for _, ch := range moveChans {
			go func() {
				ch <- move
			}()
		}
	}()

	return nil
}

func (r *repo) GetPlayers(ctx context.Context, IDs []string) map[string][]model.User {
	result := make(map[string][]model.User)

	rows, err := r.conn.Query(ctx,
		`SELECT id, "from", "to" FROM game WHERE id = ANY($1)`,
		IDs,
	)
	if err != nil {
		return result
	}
	defer rows.Close()

	for rows.Next() {
		var (
			id, fromStr, toStr string
		)
		if err := rows.Scan(&id, &fromStr, &toStr); err != nil {
			continue
		}

		result[id] = make([]model.User, 2)
		fromUser, err := model.FromString(fromStr)
		if err != nil {
			continue
		}
		result[id] = append(result[id], *fromUser)

		toUser, err := model.FromString(toStr)
		if err != nil {
			continue
		}
		result[id] = append(result[id], *toUser)
	}

	return result
}

func (r *repo) Deanonimize(ctx context.Context, ID string, user model.User, isWhite bool) error {
	var field string
	if isWhite {
		field = "from"
	} else {
		field = "to"
	}

	userStr := user.ToString()
	res, err := r.conn.Exec(
		ctx,
		fmt.Sprintf(`UPDATE game SET "%s" = $1 WHERE id = $2`, field),
		userStr, ID,
	)
	if err != nil {
		return fmt.Errorf("update failed: %w", err)
	}

	if res.RowsAffected() == 0 {
		return fmt.Errorf("game not found")
	}
	return nil
}
