"""
@Author: madrat
Application configuration primitives.
"""

from __future__ import annotations

import os
from dataclasses import dataclass, field
from pathlib import Path


@dataclass(slots=True)
class AppConfig:
    KEY_DIRECTORY: str = os.environ.get("VERIFIER_KEY_DIR", "./keys")
    PRIVATE_KEY_PATH: str = field(init=False)
    PUBLIC_KEY_PATH: str = field(init=False)

    def __post_init__(self) -> None:
        key_dir = Path(self.KEY_DIRECTORY).expanduser().resolve()
        self.KEY_DIRECTORY = str(key_dir)

        private_env = os.environ.get("VERIFIER_PRIVATE_KEY_PATH")
        public_env = os.environ.get("VERIFIER_PUBLIC_KEY_PATH")

        private_path = (
            Path(private_env).expanduser()
            if private_env
            else key_dir / "verifier_ed25519_private.pem"
        )
        public_path = (
            Path(public_env).expanduser()
            if public_env
            else key_dir / "verifier_ed25519_public.pem"
        )

        if not private_path.is_absolute():
            private_path = (key_dir / private_path).resolve()
        else:
            private_path = private_path.resolve()

        if not public_path.is_absolute():
            public_path = (key_dir / public_path).resolve()
        else:
            public_path = public_path.resolve()

        self.PRIVATE_KEY_PATH = str(private_path)
        self.PUBLIC_KEY_PATH = str(public_path)
