import base64
import json
import uuid
from datetime import datetime
from enum import StrEnum
from typing import Self

from pydantic import BaseModel


class TokenResponse(BaseModel):
    access_token: str
    token_type: str
    user_id: uuid.UUID


class MeResponse(BaseModel):
    user_id: uuid.UUID


class GirlCharacter(StrEnum):
    ART_STUDENT = "ART_STUDENT"
    OFFICE_MANAGER = "OFFICE_MANAGER"
    WEALTHY_GIRL = "WEALTHY_GIRL"


class GirlResponse(BaseModel):
    girl_id: uuid.UUID
    character: GirlCharacter
    created_at: datetime
    name: str
    is_my_girl: bool


class ChatResponse(BaseModel):
    girl_id: uuid.UUID
    session_id: uuid.UUID
    output_message: str


class PaginatedGirlsResponse(BaseModel):
    girls: list[GirlResponse]
    size: int
    current_page: int
    total_elements: int
    total_pages: int


class DatasetSample(BaseModel):
    saving_text: str
    getting_text: str


class PrivateFlagId(BaseModel):
    girl: GirlResponse
    sample: DatasetSample

    def to_base64(self) -> str:
        return base64.b64encode(self.model_dump_json().encode('utf-8')).decode('utf-8')

    @classmethod
    def from_base64(cls, based_str: str) -> Self:
        return cls(**json.loads(base64.b64decode(based_str).decode('utf-8')))
