import json

import redis.asyncio as redis

from app.config import settings
from app.schemas import GirlSecret, GirlScheme


class RedisClient:
    def __init__(self):
        self.redis_client = redis.Redis(
            host=settings.REDIS_HOST,
            port=settings.REDIS_PORT,
            db=settings.REDIS_DB,
            decode_responses=True
        )

    async def save_girl_secret(self, girl: GirlScheme, secret_data: GirlSecret) -> None:
        key = f"girl_secret:{girl.girl_id}:{girl.user_id}"
        await self.redis_client.set(key, secret_data.model_dump_json())

    async def get_girl_secret(self, girl: GirlScheme) -> GirlSecret | None:
        key = f"girl_secret:{girl.girl_id}:{girl.user_id}"
        secret_data_json = await self.redis_client.get(key)
        
        if secret_data_json is None:
            return None
            
        try:
            secret_dict = json.loads(secret_data_json)
            return GirlSecret(**secret_dict)
        except (json.JSONDecodeError, ValueError):
            return None


redis_client = RedisClient()
