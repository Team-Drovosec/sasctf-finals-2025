from datetime import datetime, timezone
from uuid import UUID

import jwt

from app.config import settings


def create_jwt(user_id: UUID) -> str:
    now = datetime.now(tz=timezone.utc)
    exp = now + settings.JWT_EXPIRE_TIME
    to_encode = {"iat": int(now.timestamp()), "exp": int(exp.timestamp()), "sub": str(user_id)}
    token = jwt.encode(to_encode, settings.JWT_SECRET, algorithm=settings.JWT_ALGORITHM)
    return token
