from typing import Any

from app.redis_client import redis_client
from app.schemas import GirlSecret


async def save_secret(secret: str, phrase: str, dependencies: dict[str, Any] | None = None) -> str:
    """
    Save secret and passphrase.
    It ensures that secrets are not saved twice.

    Args:
        secret (str): secret to save.
        phrase (str): phrase to save.
        dependencies: Available data sources (automatically provided)

    Returns:
        str: Operation status, one of the following:
            - "success": Secret and phrase successfully saved.
            - "failure": Required dependency missing, saving not possible.
            - "already saved": A secret and phrase already exist; saving skipped.
    """
    if "girl" not in dependencies:
        return "failure"

    existing_secret = await get_secret(dependencies)
    if existing_secret and "secret" in existing_secret and "phrase_saved" in existing_secret:
        return "already saved"

    girl = dependencies["girl"]
    await redis_client.save_girl_secret(
        girl,
        secret_data=GirlSecret(
            secret_phrase=phrase,
            secret_text=secret,
        )
    )
    return "success"

async def get_secret(dependencies: dict[str, Any] | None = None) -> dict | None:
    """
    Get secret and passphrase.

    Args:
        dependencies: Available data sources (automatically provided)

    Returns:
        dict | None: One of the following states:
            - {"secret": str, "phrase_saved": str} — Successfully retrieved secret and passphrase.
            - {"error": "error"} — Returned when retrieval failed due to context issues.
            - None — No secret was previously saved.
    """
    if "girl" not in dependencies:
        return {"error": "error"}
    girl = dependencies["girl"]
    result = await redis_client.get_girl_secret(girl)
    if not result:
        return None
    return {"secret": result.secret_text, "phrase_saved": result.secret_phrase}
