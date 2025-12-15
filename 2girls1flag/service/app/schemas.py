import uuid
from datetime import datetime

from pydantic import BaseModel, Field, ConfigDict

from app.models import GirlCharacter


class TokenResponse(BaseModel):
    access_token: str = Field(..., description="JWT access token")
    token_type: str = "bearer"
    user_id: uuid.UUID


class AuthRequest(BaseModel):
    user_id: uuid.UUID | None = None


class MeResponse(BaseModel):
    user_id: uuid.UUID


class GirlScheme(BaseModel):
    model_config = ConfigDict(from_attributes=True)

    girl_id: uuid.UUID
    name: str
    user_id: uuid.UUID
    character: GirlCharacter


class GirlResponse(BaseModel):
    model_config = ConfigDict(from_attributes=True)

    girl_id: uuid.UUID
    name: str
    character: GirlCharacter
    is_my_girl: bool = True
    created_at: datetime


class GirlSecret(BaseModel):
    secret_phrase: str
    secret_text: str


class ChatRequest(BaseModel):
    girl_id: uuid.UUID
    session_id: uuid.UUID | None = None
    message: str = Field(..., min_length=2, max_length=1000)


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
