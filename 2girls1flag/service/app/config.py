import os
from datetime import timedelta

from pydantic import computed_field, Field
from pydantic_settings import BaseSettings


class Settings(BaseSettings):
    APP_NAME: str = "2girls1flag"
    DEBUG: bool = False

    POSTGRES_USER: str
    POSTGRES_PASSWORD: str
    POSTGRES_HOST: str
    POSTGRES_PORT: int
    POSTGRES_DB: str = Field("2girls1flag_db")
    POSTGRES_AGENT_MEMORY_DB: str = Field("agent_db")

    REDIS_HOST: str = "localhost"
    REDIS_PORT: int = 6379
    REDIS_DB: int = 0

    JWT_SECRET: bytes = os.urandom(64)
    JWT_ALGORITHM: str = "HS256"
    JWT_EXPIRE_TIME: timedelta = timedelta(minutes=60*24)

    AI_URL: str = Field(...)
    AI_MODEL: str = Field(default="Qwen/Qwen3-VL-30B-A3B-Instruct")
    AI_MAX_TOKENS: int = Field(default=3_000)

    @computed_field
    @property
    def PG_URL(self) -> str:
        return (
            f"postgresql+asyncpg://{self.POSTGRES_USER}:{self.POSTGRES_PASSWORD}"
            f"@{self.POSTGRES_HOST}:{self.POSTGRES_PORT}/{self.POSTGRES_DB}"
        )

    @computed_field
    @property
    def AGENT_PG_URL(self) -> str:
        return (
            f"postgresql://{self.POSTGRES_USER}:{self.POSTGRES_PASSWORD}"
            f"@{self.POSTGRES_HOST}:{self.POSTGRES_PORT}/{self.POSTGRES_DB}"
        )

    class Config:
        env_file = ".env"
        env_file_encoding = "utf-8"


settings = Settings()
