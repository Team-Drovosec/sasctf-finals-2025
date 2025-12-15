import uuid
from uuid import UUID

import requests
from checklib import BaseChecker, Status

import config
from schemas import TokenResponse, MeResponse, GirlResponse, PaginatedGirlsResponse, ChatResponse


class ServiceClient:
    def __init__(self, checker: BaseChecker, port: int | str, session: requests.Session):
        self.url = f"http://{checker.host}:{port}"
        self.session = session
        self.checker = checker

    def auth(self, user_id: str | None = None, timeout: float = 10) -> TokenResponse:
        resp = self.session.post(
            f"{self.url}/auth",
            json={
                "user_id": user_id,
            },
            timeout=timeout,
        )
        self.checker.assert_(resp.ok, "Failed to auth", status=Status.MUMBLE)
        result: TokenResponse = self._cast(resp.json(), TokenResponse)
        if access_token := result.access_token:
            self.session.headers.update({"Authorization": f"Bearer {access_token}"})
        return result

    def me(self, timeout: float = 10) -> MeResponse:
        resp = self.session.get(f"{self.url}/me", timeout=timeout)
        self.checker.assert_(resp.ok, "Failed to get user info", status=Status.MUMBLE)
        return self._cast(resp.json(), MeResponse)

    def get_girls(self, page: int = 1, size: int = 10, timeout: float = 10) -> PaginatedGirlsResponse:
        resp = self.session.get(f"{self.url}/girls", params={"page": page, "size": size}, timeout=timeout)
        self.checker.assert_(resp.ok, "Failed to get created girls", status=Status.MUMBLE)
        return self._cast(resp.json(), PaginatedGirlsResponse)

    def get_all_girls(self, page: int = 1, size: int = 10, timeout: float = 10) -> PaginatedGirlsResponse:
        resp = self.session.get(f"{self.url}/all_girls", params={"page": page, "size": size}, timeout=timeout)
        self.checker.assert_(resp.ok, "Failed to get all girls", status=Status.MUMBLE)
        return self._cast(resp.json(), PaginatedGirlsResponse)

    def create_girl(self, timeout: float = 10) -> GirlResponse:
        resp = self.session.post(f"{self.url}/girls", timeout=timeout)
        self.checker.assert_(resp.ok, "Failed to create girl", status=Status.MUMBLE)
        return self._cast(resp.json(), GirlResponse)

    def chat(self, *, girl_id: UUID, message: str, session_id: UUID | None = None, timeout: float = config.CHAT_REQUEST_TIMEOUT.total_seconds()) -> ChatResponse:
        resp = self.session.post(
            f"{self.url}/chat",
            json={
                "girl_id": str(girl_id),
                "message": message,
                "session_id": str(session_id) if session_id else None,
            },
            timeout=timeout,
        )
        self.checker.assert_(resp.ok, "Failed to create chat message", status=Status.MUMBLE)
        return self._cast(resp.json(), ChatResponse)

    def _cast(self, data: dict, pydantic_type):
        try:
            return pydantic_type(**data)
        except:
            self.checker.cquit(
                Status.MUMBLE,
                "Response parsing error",
                private=f"Cannot convert api-response to {pydantic_type}",
            )
            raise
