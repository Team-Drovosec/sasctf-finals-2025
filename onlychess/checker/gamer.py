#!/bin/env python3

import logging
import random
import sys
import time
import os
from typing import IO, Tuple, Optional

import chess
import chess.engine

from checklib import *
from client import ChessHubClient
from config import random_string, WEBSOCKET_WAIT_SECONDS, Mumble, ANON_FMT, remarks, end_remarks

STOCKFISH_EXECUTABLES = [
    "./stockfish",
    "stockfish",
    "/usr/games/stockfish",
    "/usr/local/bin/stockfish",
    "/usr/bin/stockfish"
]

def find_stockfish() -> Optional[str]:
    """Finds a usable Stockfish executable from a list of common paths."""
    for path in STOCKFISH_EXECUTABLES:
        if os.path.exists(path) and os.access(path, os.X_OK):
            logging.info(f"Found Stockfish executable at: {path}")
            return path
    return None

over_games = set()

class Gamer(BaseChecker):

    def __init__(self, *args, **kwargs):
        super(Gamer, self).__init__(*args, **kwargs)
        self.base_url = f"http://{self.host}:4000"

        # Find Stockfish on initialization
        self.stockfish_path = find_stockfish()
        if not self.stockfish_path:
            # Using self.cquit for checker-friendly exiting
            self.cquit(Status.ERROR, "Chess engine (Stockfish) not found or not executable.", "Stockfish not found")


    def register_login_player(self, client: ChessHubClient):
        username = "chess-hustler-" + random_string(3)
        password = random_string()
        client.register(username, password)
        client.login(username, password)

        idx = client.get_index()
        logging.info(f"Successfully registered and logged in as {username}")

    def gaming(self, client: ChessHubClient):
        self.register_login_player(client)
        client.create_open_lobby()
        logging.info(f"Created open lobby")

        engine = None
        while True:
            try:
                engine = chess.engine.SimpleEngine.popen_uci(self.stockfish_path)
                logging.info("Chess engine started successfully.")

                # Main game loop with a timeout to prevent infinite execution
                start_time = time.time()
                timeout_seconds = 210
                move_made = False

                while time.time() - start_time < timeout_seconds:
                    games = [g["ID"] for g in client.get_recent_games()]
                    if not games:
                        logging.info("Waiting for an opponent to join the game...")
                        time.sleep(WEBSOCKET_WAIT_SECONDS)
                        continue

                    my_turn_found = False
                    for game_id in games:
                        if game_id in over_games:
                            continue
                        state = client.get_game_state(game_id)
                        client.connect_to_chat_ws(game_id)
                        time.sleep(WEBSOCKET_WAIT_SECONDS)
                        fen = state["fen"]
                        board = chess.Board(fen)

                        # Check if the game is over and if it is our turn
                        if not board.is_game_over() and board.turn == chess.WHITE:
                            # if random.randint(0, 100) < 25:
                            client.send_public_chat_message(game_id, random.choice(remarks))
                            logging.info(f"Our turn to move in game {game_id}. FEN: {fen}")

                            # Ask the engine for the best move with a short time limit
                            # A short limit is crucial for a checker to not time out.
                            result = engine.play(board, chess.engine.Limit(time=0.2))
                            best_move = result.move

                            # The client expects from_square and to_square separately
                            from_square = chess.square_name(best_move.from_square)
                            to_square = chess.square_name(best_move.to_square)

                            logging.info(f"Engine suggests move: {best_move.uci()} ({from_square} -> {to_square})")
                            client.make_move(game_id, from_square, to_square)

                            move_made = True
                            my_turn_found = True
                            # Break from the inner loop after making a move for this turn
                            break
                        elif board.is_game_over():
                            logging.info(f"Game {game_id} is over. Result: {board.result()}")
                            over_games.add(game_id)

                            client.create_open_lobby()
                            continue

                    if my_turn_found:
                        # Wait a bit for the opponent to make their move
                        logging.info("Move made, waiting for opponent...")

                    time.sleep(WEBSOCKET_WAIT_SECONDS)

                if not move_made:
                    self.cquit(Status.MUMBLE, "Failed to make any move within the time limit.", "No moves made in game")
                else:
                    # If we made moves but the game didn't end, it's still OK
                    self.cquit(Status.OK)

            except chess.engine.EngineTerminatedError:
                logging.error("Chess engine process terminated unexpectedly.", "EngineTerminatedError")
            except Exception as e:
                logging.error(f"An unexpected error occurred during the gaming check: {e}")
            finally:
                if engine:
                    logging.info("Quitting chess engine.")
                    engine.quit()
                client.send_public_chat_message(game_id, random.choice(end_remarks))
                over_games.add(game_id)
                client.create_open_lobby()


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

    host = sys.argv[1]
    while True:
        c = Gamer(host)
        test_client = ChessHubClient(f"http://{host}:4000")
        c.gaming(test_client)
