import logging
import random
import re
import json
import threading
from typing import Dict, Any, List
from urllib.parse import urlparse

import requests
import websocket

import config
from config import Mumble, ANON_FMT


class ChessHubClient:
    def __init__(self, base_url):
        self.base_url = base_url
        self.session = requests.Session()
        self.session.headers.update({
            "User-Agent": random.choice(config.USER_AGENTS),
            'Content-Type': 'application/json',
            'Accept': 'application/json, text/html'
        })
        self.username = None
        self.anon_id = None
        self.passwd = None

        self.ws_connections = {}  # Stores WebSocketApp objects: {game_id: {type: ws_app}}
        self.ws_threads = {}      # Stores thread objects: {game_id: {type: thread}}
        self.ws_messages = {}     # Stores received messages: {game_id: {type: [messages]}}
        logging.info(f"Client initialized for target: {self.base_url}")

    def _handle_api_error(self, response, action_name="API call"):
        """Centralized helper to log errors from API responses."""
        logging.error(f"{action_name} failed. Status: {response.status_code if response else -1}")
        try:
            if response:
                error_msg = response.json().get("message", "No error message provided.")
                logging.error(f"Server response: {error_msg}")
            else:
                logging.error(f"Could not connect to the server")
        except requests.exceptions.JSONDecodeError:
            logging.error(f"Server response: {response.text}")

    def register(self, username, password):
        register_url = f"{self.base_url}/register"
        payload = {"username": username, "password": password}
        logging.info(f"Attempting to register user: '{username}':'{password}'")
        try:
            response = self.session.post(register_url, json=payload)
            response.raise_for_status()
            logging.info(f"Successfully registered user '{username}'.")
            self.username = username
            self.passwd = password
            return True
        except requests.exceptions.RequestException as e:
            self._handle_api_error(e.response, "Registration")
            raise e

    def login(self, username, password):
        login_url = f"{self.base_url}/login"
        payload = {"username": username, "password": password}
        logging.info(f"Attempting to log in as user: '{username}'")
        try:
            response = self.session.post(login_url, json=payload)
            response.raise_for_status()
            logging.info(f"Successfully logged in as '{username}'.")
            self.username = username
            self.anon_id = None
            return True
        except requests.exceptions.RequestException as e:
            self._handle_api_error(e.response, "Login")
            raise e


    def get_index(self):
        home_url = f"{self.base_url}/"
        logging.info(f"Checking the index.html of: '{self.username}'")
        try:
            response = self.session.get(home_url)
            response.raise_for_status()

            return response.text
        except requests.exceptions.RequestException as e:
            self._handle_api_error(e.response, "Login")
            raise e

    def create_open_lobby(self):
        url = f"{self.base_url}/open-invite-create"
        logging.info(f"User '{self.username}' is creating an open lobby.")
        try:
            response = self.session.post(url, json={})
            response.raise_for_status()
            logging.info("Successfully created open lobby.")
            return True
        except requests.exceptions.RequestException as e:
            self._handle_api_error(e.response, "Create open lobby")
            raise e

    def challenge_player(self, opponent_username):
        url = f"{self.base_url}/invite"
        payload = {"to": opponent_username}
        logging.info(f"User '{self.username}' is challenging '{opponent_username}'.")
        try:
            response = self.session.post(url, json=payload)
            response.raise_for_status()
            logging.info(f"Successfully challenged '{opponent_username}'.")
            return True
        except requests.exceptions.RequestException as e:
            self._handle_api_error(e.response, f"Challenge player '{opponent_username}'")
            raise e

    def get_open_lobbies(self):
        url = f"{self.base_url}/open-invites"
        try:
            response = self.session.get(url)
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            self._handle_api_error(e.response, "Get open lobbies")
            raise e

    def get_my_challenges(self):
        url = f"{self.base_url}/invites"
        try:
            response = self.session.get(url)
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            self._handle_api_error(e.response, "Get my challenges")
            raise e

    def get_recent_games(self):
        url = f"{self.base_url}/recent-games"
        try:
            response = self.session.get(url)
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            self._handle_api_error(e.response, "Get recent games")
            raise e

    def _get_player_api_name(self, player_object):
        if player_object.get("Username"):
            return player_object["Username"]
        if player_object.get("AnonymousID"):
            return f"anon:{player_object['AnonymousID']}"
        return ""

    def _respond_to_invite(self, endpoint, from_username, action_name):
        url = f"{self.base_url}{endpoint}"
        payload = {"from": from_username}
        logging.info(f"User '{self.username}' is attempting to {action_name.lower()} challenge from '{from_username}'.")
        try:
            response = self.session.post(url, json=payload)
            response.raise_for_status()
            logging.info(f"Successfully {action_name.lower()}ed challenge from '{from_username}'.")
            if "accept" in action_name.lower():
                return response.text.strip('"')
            return True
        except requests.exceptions.RequestException as e:
            self._handle_api_error(e.response, f"{action_name} challenge from '{from_username}'")
            raise e

    def accept_challenge(self, from_username):
        return self._respond_to_invite("/invite-accept", from_username, "Accept")

    def decline_challenge(self, from_username):
        return self._respond_to_invite("/invite-decline", from_username, "Decline")

    def join_open_lobby(self, api_name):
        payload = {"from": api_name}
        logging.info(f"User '{self.username}' is attempting to join lobby of '{api_name}'.")
        try:
            response = self.session.post(f"{self.base_url}/open-invite-accept", json=payload)
            if response.status_code == 404:
                raise FileNotFoundError()
            response.raise_for_status()
            game_id = response.text.strip('"')
            logging.info(f"Successfully joined lobby. Game ID: {game_id}")
            return game_id
        except requests.exceptions.RequestException as e:
            self._handle_api_error(e.response, f"Join lobby of '{api_name}'")
            raise e

    def get_anon_id(self) -> int:
        url = f"{self.base_url}/"
        logging.info(f"Getting / to get anon ID")
        try:
            response = self.session.get(url)
            response.raise_for_status()
            html_content = response.text
            anon_id = re.search(r'<span class="hidden" id="anon-id">(.*?)</span>', html_content)
            if not anon_id:
                raise Mumble("Anon ID if missing from the page", "")

            aid  =int(anon_id.group(1).strip())
            self.anon_id = aid
            self.username = ANON_FMT.format(id=aid)

            return aid
        except requests.exceptions.RequestException as e:
            self._handle_api_error(e.response, f"Get anon id")
            raise e

    def get_game_state(self, game_id: str) -> Dict[str, Any]:
        url = f"{self.base_url}/game?id={game_id}"
        logging.info(f"Fetching game state for game ID: {game_id}")
        try:
            response = self.session.get(url)
            response.raise_for_status()
            html_content = response.text
            fen_match = re.search(r'const initialFEN = "([^"]+)";', html_content)
            status_match = re.search(r'let gameInProgress = "([^"]+)" === "started";', html_content)
            white_player_match = re.search(r'<span id="white-player".*?>White: (.*?)</span>', html_content)
            black_player_match = re.search(r'<span id="black-player".*?>Black: (.*?)</span>', html_content)
            if not all([fen_match, status_match, white_player_match, black_player_match]):
                raise Mumble("game html page is invalid", "")
            return {
                "id": game_id, "fen": fen_match.group(1).replace("\\", ""), "status": status_match.group(1),
                "white_player": white_player_match.group(1).strip(),
                "black_player": black_player_match.group(1).strip(),
            }
        except requests.exceptions.RequestException as e:
            self._handle_api_error(e.response, f"Get game state for '{game_id}'")
            raise e

    def make_move(self, game_id: str, from_square: str, to_square: str) -> bool:
        """
        Submits a move for a given game in algebraic notation (e.g., 'e2', 'e4').
        """
        url = f"{self.base_url}/move"
        payload = {"game": game_id, "from": from_square, "to": to_square}
        logging.info(f"User '{self.username}' is making move {from_square}-{to_square} in game {game_id}.")
        try:
            response = self.session.post(url, json=payload)
            response.raise_for_status()
            logging.info(f"Move {from_square}-{to_square} successful.")
            return True
        except requests.exceptions.RequestException as e:
            logging.error(f"Make move in game '{game_id}' failed. Status: {e.response.status_code}")
            logging.error(f"Server response: {e.response.text}")
            raise e

    def _get_ws_url(self, path):
        """Constructs a WebSocket URL from the base HTTP URL."""
        parsed_url = urlparse(self.base_url)
        scheme = 'wss' if parsed_url.scheme == 'https' else 'ws'
        return f"{scheme}://{parsed_url.netloc}{path}"

    def _ws_on_message(self, ws, message, game_id, ws_type):
        """Callback to store received WebSocket messages."""
        logging.info(f"WS message received for game '{game_id}' ({ws_type}): {message}")
        try:
            parsed_message = json.loads(message)
            self.ws_messages[game_id][ws_type].append(parsed_message)
        except json.JSONDecodeError:
            logging.warning(f"Received non-JSON WS message: {message}")
            self.ws_messages[game_id][ws_type].append({"raw": message})


    def _ws_on_error(self, ws, error):
        """Callback to log WebSocket errors."""
        logging.error(f"WebSocket error: {error}")

    def _ws_on_close(self, ws, close_status_code, close_msg):
        """Callback to log WebSocket connection closure."""
        logging.info(f"WebSocket connection closed. Code: {close_status_code}, Msg: {close_msg}")

    def _start_ws_connection(self, game_id: str, ws_type: str, path: str):
        """Generic helper to establish a WebSocket connection."""
        if game_id not in self.ws_connections:
            self.ws_connections[game_id] = {}
            self.ws_threads[game_id] = {}
            self.ws_messages[game_id] = {}

        if ws_type in self.ws_connections[game_id]:
             logging.warning(f"A {ws_type} WebSocket for game '{game_id}' already exists.")
             return

        self.ws_messages[game_id][ws_type] = []

        ws_url = self._get_ws_url(f"{path}?id={game_id}")
        cookies = "; ".join([f"{c.name}={c.value}" for c in self.session.cookies])

        ws_app = websocket.WebSocketApp(
            ws_url,
            on_message=lambda ws, msg: self._ws_on_message(ws, msg, game_id, ws_type),
            on_error=self._ws_on_error,
            on_close=self._ws_on_close,
            cookie=cookies
        )
        self.ws_connections[game_id][ws_type] = ws_app

        thread = threading.Thread(target=ws_app.run_forever)
        thread.daemon = True
        self.ws_threads[game_id][ws_type] = thread
        thread.start()
        logging.info(f"Started {ws_type} WebSocket listener for game '{game_id}' on URL: {ws_url}")

    def connect_to_game_ws(self, game_id: str):
        """Connects to the game move WebSocket and starts listening for moves."""
        self._start_ws_connection(game_id, 'game', '/next-moves')

    def get_game_moves(self, game_id: str) -> List[Dict[str, Any]]:
        """Retrieves all recorded game moves for a game and clears the buffer."""
        if game_id in self.ws_messages and 'game' in self.ws_messages[game_id]:
            messages = self.ws_messages[game_id]['game'][:]
            self.ws_messages[game_id]['game'] = []
            return messages
        return []

    def connect_to_chat_ws(self, game_id: str):
        """Connects to the chat WebSocket and starts listening for messages."""
        self._start_ws_connection(game_id, 'chat', '/chat')

    def get_chat_messages(self, game_id: str) -> List[Dict[str, Any]]:
        """Retrieves all recorded chat messages for a game and clears the buffer."""
        if game_id in self.ws_messages and 'chat' in self.ws_messages[game_id]:
            messages = self.ws_messages[game_id]['chat'][:]
            self.ws_messages[game_id]['chat'] = []
            return messages
        return []

    def send_public_chat_message(self, game_id: str, message: str):
        """Sends a public (spectator) message via the chat WebSocket."""
        if game_id not in self.ws_connections or 'chat' not in self.ws_connections[game_id]:
            logging.error(f"No active chat WebSocket for game '{game_id}'. Call connect_to_chat_ws first.")
            return

        ws_app = self.ws_connections[game_id]['chat']
        payload = {
            "user": self.username or 'Anonymous',
            "is_private": False,
            "content": message
        }
        try:
            ws_app.send(json.dumps(payload))
            logging.info(f"Sent public chat message to game '{game_id}': {message}")
        except Exception as e:
            logging.error(f"Failed to send public chat message to game '{game_id}': {e}")

    def send_private_chat_message(self, game_id: str, message: str) -> bool:
        """Sends a private (game) message via an HTTP POST request."""

        url = f"{self.base_url}/private-chat"
        payload = {"game": game_id, "message": message}
        logging.info(f"User '{self.username}' is sending private chat message in game {game_id}.")
        try:
            response = self.session.post(url, json=payload)
            response.raise_for_status()
            logging.info(f"Private chat message sent successfully.")
            return True
        except requests.exceptions.RequestException as e:
            self._handle_api_error(e.response, f"Send private chat message in game '{game_id}'")
            raise e

    def get_secret_chat_key(self, game_id: str) -> List[int]:
        """Sends a private (game) message via an HTTP POST request."""

        url = f"{self.base_url}/chat-key?id={game_id}"
        logging.info(f"User '{self.username}' getting private key for the game {game_id}.")
        try:
            response = self.session.get(url)
            response.raise_for_status()
            logging.info(f"Private key got.")
            return response.json()
        except requests.exceptions.RequestException as e:
            self._handle_api_error(e.response, f"User '{self.username}' getting private key for the game {game_id}.")
            raise e

    def close_all_websockets(self):
        """Closes all active WebSocket connections."""
        logging.info("Closing all active WebSocket connections...")
        for game_id in self.ws_connections:
            for ws_type in self.ws_connections[game_id]:
                ws_app = self.ws_connections[game_id][ws_type]
                if ws_app:
                    ws_app.close()
        self.ws_connections.clear()
        self.ws_threads.clear()
        self.ws_messages.clear()
        logging.info("All WebSockets closed.")
