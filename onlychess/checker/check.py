#!/bin/env python3

import asyncio
import base64
import dataclasses
import json
import logging
import os
import random
import sys
import time
from typing import IO, Tuple

import requests
from checklib import *
from checklib import status

from cipher import StreamCipher
from cipher.utils import base64_to_bit_array, bits_to_string
from client import ChessHubClient
from config import random_string, WEBSOCKET_WAIT_SECONDS, Mumble, ANON_FMT


class Checker(BaseChecker):
    down_exceptions = {
        requests.exceptions.ConnectionError,
        requests.exceptions.ProxyError,
        requests.exceptions.Timeout,
        requests.exceptions.ReadTimeout,
        requests.exceptions.TooManyRedirects,
        ConnectionError,
    }
    mumble_exceptions = {
        requests.exceptions.JSONDecodeError,
        requests.exceptions.ChunkedEncodingError,
        requests.exceptions.ContentDecodingError,
        json.decoder.JSONDecodeError,
    }

    def __init__(self, *args, **kwargs):
        super(Checker, self).__init__(*args, **kwargs)
        self.base_url = f"http://{self.host}:4000"

    def assert_function(self, condition: bool, error_message: str, private: str = None):
        """
        Custom assertion function. If the condition is false, it raises a Mumble exception.

        :param condition: The boolean condition to check.
        :param error_message: The public and private error message to use if the assertion fails.
        """
        if private is None:
            private = error_message
        if not condition:
            raise Mumble(public=error_message, private=private)

    def action(self, action, *args, **kwargs):
        try:
            return super(Checker, self).action(action, *args, **kwargs)
        except Exception as exc:
            exc_type = type(exc)
            if exc_type in self.down_exceptions:
                self.cquit(Status.DOWN, "Service is down", private=str(exc))
            if exc_type in self.mumble_exceptions:
                self.cquit(Status.MUMBLE, "Service is mumbling", private=str(exc))
            if exc_type is Mumble:
                self.cquit(Status.MUMBLE, exc.public, exc.private)
            raise

    def register_login_player(self, client: ChessHubClient):
        username = random_string()
        password = random_string()
        client.register(username, password)
        client.login(username, password)
        self.assert_function(client.username == username, "Client 1 failed to login")

        idx = client.get_index()
        self.assert_function(
            f"""<span id="current-user" class="font-medium dark:text-gray-200">{username}</span>""" in idx,
            "Failed to login, no username in site header"
        )

    def register_2_users_and_play_a_game(self) -> Tuple[ChessHubClient, ChessHubClient, str]:
        # Create two separate clients for two users.
        client1 = ChessHubClient(self.base_url)
        client2 = ChessHubClient(self.base_url)

        self.register_login_player(client1)
        self.register_login_player(client2)

        return self.test_gaming(client1, client2, False)

    def test_gaming(
            self,
            client1: ChessHubClient,
            client2: ChessHubClient,
            skip_c2_to_c1=False
    ) -> Tuple[ChessHubClient, ChessHubClient, str]:
        # Player 1 (challenger) sends a direct invite to Player 2
        client1.challenge_player(client2.username)

        c2_challs = client2.get_my_challenges()
        self.assert_function(client1.username in str(c2_challs) or str(client1.anon_id) in str(c2_challs),
                             "New challenge between two users isn't available")

        c1_recent_games = client1.get_recent_games()
        self.assert_function(len(c1_recent_games) == 0, "Recent games are not zero for some reason for a fresh user")
        c2_recent_games = client2.get_recent_games()
        self.assert_function(len(c2_recent_games) == 0, "Recent games are not zero for some reason for a fresh user")

        # Player 2 (challenged) accepts the invite. This returns the game ID.
        game_id = client2.accept_challenge(client1.username)
        self.assert_function(game_id is not None, "Accepting a challenge should return a valid game ID")
        self.assert_function(len(game_id) > 10, f"Received an invalid or too short game ID: {game_id}")

        self.test_play_game(client1, client2, game_id)

        # decline challenge
        if not skip_c2_to_c1:
            client2.challenge_player(client1.username)

            c1_challs = client1.get_my_challenges()
            self.assert_function(client2.username in str(c1_challs), "New challenge between two users isn't available")

            client1.decline_challenge(client2.username)

            с1_recent_games = client1.get_recent_games()
            self.assert_function(len(с1_recent_games) == 1,
                                 "Didn't decline challenge or somthing wrong with /recent-games")
            с2_recent_games = client2.get_recent_games()
            self.assert_function(len(с2_recent_games) == 1,
                                 "Didn't decline challenge or somthing wrong with /recent-games")

            c1_challs = client1.get_my_challenges()
            self.assert_function(client2.username not in str(c1_challs), "Declining a challenge didn't remove it")

        return client1, client2, game_id

    def test_play_game(self, client1: ChessHubClient, client2: ChessHubClient, game_id: str):
        c1_recent_games = client1.get_recent_games()
        self.assert_function(len(c1_recent_games) == 1, "New game didn't apper in recent games list")
        c2_recent_games = client2.get_recent_games()
        self.assert_function(len(c2_recent_games) == 1, "New game didn't apper in recent games list")

        client1.connect_to_game_ws(game_id)
        client2.connect_to_game_ws(game_id)

        key = client1.get_secret_chat_key(game_id)
        cip = StreamCipher(key)

        # test chat
        test_message = random_string()
        client1.connect_to_chat_ws(game_id)
        client2.connect_to_chat_ws(game_id)

        time.sleep(WEBSOCKET_WAIT_SECONDS)
        if client1.anon_id is None and client2.anon_id is None:  # If users are anonyms, they could not be able to chat, its ok
            client1.send_public_chat_message(game_id, "Hello GLHF")
            client2.send_public_chat_message(game_id, "Hi")
            client1.send_public_chat_message(game_id, test_message)
            time.sleep(WEBSOCKET_WAIT_SECONDS)
            messages = client2.get_chat_messages(game_id)
            self.assert_function(test_message in str(messages), "The public chat message wasn't recieved")

            secret_text = random_string()
            client2.send_private_chat_message(game_id, secret_text)

            time.sleep(WEBSOCKET_WAIT_SECONDS)
            msgs = client1.get_chat_messages(game_id)
            last_private_msg = [m for m in msgs if m["is_private"]][-1]
            payload = base64_to_bit_array(last_private_msg["Content"])
            iv = last_private_msg["iv"]

            decoded = bits_to_string(cip.crypt(payload, iv)[0])
            self.assert_function(decoded == secret_text, "Decrypted message from private chat does not match")

        # Player 1 (White) makes the first move: e2-e4
        client1.make_move(game_id, "e2", "e4")

        # Verify the game state reflects the move. Check that it is now Black's turn.
        game_state_after_e4 = client2.get_game_state(game_id)
        self.assert_function(game_state_after_e4 is not None, "Failed to get game state after the first move")
        self.assert_function(" b " in game_state_after_e4["fen"],
                             "Game state FEN is incorrect: should be Black's turn.")

        # Player 2 (Black) responds with e7-e5
        client2.make_move(game_id, "e7", "e5")

        # Verify the game state again. Check that it is now White's turn.
        game_state_after_e5 = client1.get_game_state(game_id)
        self.assert_function(game_state_after_e5 is not None, "Failed to get game state after the second move")
        self.assert_function(" w " in game_state_after_e5["fen"],
                             "Game state FEN is incorrect: should be White's turn.")

        c1_moves = client1.get_game_moves(game_id)
        c2_moves = client2.get_game_moves(game_id)
        self.assert_function(c1_moves == c2_moves, 'Users got different moves in the websocket')

    def register_user_and_watch_game(self, game_id, client=None):
        if not client:
            client = ChessHubClient(self.base_url)
            self.register_login_player(client)

        recent_0 = client.get_recent_games()

        game = client.get_game_state(game_id)
        recent_1 = client.get_recent_games()
        self.assert_function(recent_1 == recent_0, "Recent games randomly changed")

        client.connect_to_game_ws(game_id)
        client.connect_to_chat_ws(game_id)
        time.sleep(WEBSOCKET_WAIT_SECONDS)

        tx = random_string()
        client.send_public_chat_message(game_id, tx)

        recent_2 = client.get_recent_games()
        self.assert_function(any([x["ID"] == game_id for x in recent_2]),
                             "After sending a message to the chat, the game didn't appear in recent games for a third person")

        time.sleep(WEBSOCKET_WAIT_SECONDS)
        chat = client.get_chat_messages(game_id)
        self.assert_function(tx in str(chat), "Didn't get the message sent to public chat")

    def check_anon_vs_player(self) -> Tuple[ChessHubClient, ChessHubClient, str]:
        client1 = ChessHubClient(self.base_url)
        client2 = ChessHubClient(self.base_url)

        self.register_login_player(client1)
        client2.get_anon_id()

        try:
            return self.test_gaming(client2, client1, False)
        except Mumble as exc:
            raise Mumble("Player vs Anon: " + exc.public, exc.private)

    def check_deanonimize(self):
        client1, client2, game = self.check_anon_vs_player()  # c2 white, c1 black. c2 is anon

        game_state_0 = client1.get_game_state(game)
        self.register_login_player(client1)
        game_state_1 = client1.get_game_state(game)
        self.assert_function(
            (
                    game_state_1["black_player"] == game_state_0["black_player"] and
                    game_state_1["id"] == game_state_0["id"] and
                    game_state_1["fen"] == game_state_0["fen"] and
                    game_state_1["status"] == game_state_0["status"]
            ),
            "game state changed incorrectly after deanonimization",
            json.dumps({
                "game_state_0": game_state_0,
                "game_state_1": game_state_1
            })
        )
        self.assert_function(
            'anon:' in game_state_0["white_player"] and client1.username == game_state_1["white_player"],
            "Could not deanonimize the game on login of one of the users",
            json.dumps({
                "game_state_0": game_state_0,
                "game_state_1": game_state_1
            })
        )

        # todo continue game, play one more move

    def check_two_anonyms_open_lobby(self):
        client1 = ChessHubClient(self.base_url)
        client2 = ChessHubClient(self.base_url)

        client1.get_anon_id()
        client2.get_anon_id()

        is404 = True
        attempt = 1
        game = None
        while is404 and attempt < 10:
            is404 = False
            try:
                client1.create_open_lobby()
                lobbies = client2.get_open_lobbies()
                game = client2.join_open_lobby(ANON_FMT.format(id=client1.anon_id))
            except FileNotFoundError:
                is404 = True
                attempt += 1

        if game is None:
            raise Mumble("Failed to join open lobby", "")

        self.test_play_game(client1, client2, game)

    def check(self):
        client1, client2, game = self.register_2_users_and_play_a_game()
        self.register_user_and_watch_game(game)
        self.check_anon_vs_player()
        self.check_two_anonyms_open_lobby()
        self.check_deanonimize()

        self.cquit(Status.OK, "Service is fully functional")

    def put(self, flag_id: str, flag: str, vuln: str):
        put_type = random.randint(0, 2)
        if put_type == 0:
            # player vs player
            client1, client2, game = self.register_2_users_and_play_a_game()
        else:
            # player vs anon
            client1, client2, game = self.check_anon_vs_player() # 2 is anon

        author = client1
        if put_type == 0 and random.randint(0, 2) == 0:
            author = client2

        key = client1.get_secret_chat_key(game)
        cip = StreamCipher(key)
        key2 = client2.get_secret_chat_key(game)
        self.assert_function(key == key2, "Private Key sent to the second player is different or non-existent")

        author.send_private_chat_message(game, flag)
        time.sleep(WEBSOCKET_WAIT_SECONDS)

        msgs = client1.get_chat_messages(game)
        msgs2 = client2.get_chat_messages(game)
        self.assert_function(msgs == msgs2, "The second player didn't get some private messages the first got")

        last_private_msg = [m for m in msgs if m["is_private"]][-1]
        payload = base64_to_bit_array(last_private_msg["Content"])
        iv = last_private_msg["iv"]

        decoded = bits_to_string(cip.crypt(payload, iv)[0])
        self.assert_function(decoded == flag, "Decrypted message from private chat does not match")

        self.cquit(
            Status.OK,
            public=game,
            private=base64.b64encode(json.dumps({
                "game": game,
                "players": [
                    [p.username, p.passwd]
                    for p in [client1, client2] if p.anon_id is None
                ]
            }).encode()).decode()
        )

    def get(self, flag_id: str, flag: str, vuln: str):
        flag_id_ = base64.b64decode(flag_id)
        fl = json.loads(flag_id_)
        game = fl["game"]
        players = fl["players"]
        try:
            for pl in players:
                client = ChessHubClient(self.base_url)
                client.login(pl[0], pl[1])
                client.get_game_state(game)

                key = client.get_secret_chat_key(game)
                cip = StreamCipher(key)

                client.connect_to_chat_ws(game)
                time.sleep(WEBSOCKET_WAIT_SECONDS)
                msgs = [m for m in client.get_chat_messages(game) if m["is_private"]]
                msgs_decoded = [
                    bits_to_string(
                        cip.crypt(
                            base64_to_bit_array(m["Content"]),
                            m["iv"]
                        )[0]
                    )
                    for m in msgs
                ]
                if not any([m == flag for m in msgs_decoded]):
                    self.cquit(Status.CORRUPT)
        except requests.exceptions.RequestException:
            self.cquit(Status.CORRUPT)

        self.cquit(Status.OK)


if __name__ == '__main__':
    logging.basicConfig(level=logging.CRITICAL, format='%(asctime)s - %(levelname)s - %(message)s')

    attempt = 1


    c = Checker(sys.argv[2])
    while True:
        logging.info(f"ATTEMPT {attempt}")
        try:
            c.action(sys.argv[1], *sys.argv[3:])
        except c.get_check_finished_exception() as e:
            pass

        if c.status == Status.OK or c.status == 101:
            break
        if attempt >= 5:
            break
        attempt += 1

    cquit(status.Status(c.status), c.public, c.private)
