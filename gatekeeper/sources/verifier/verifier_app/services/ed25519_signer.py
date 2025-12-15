"""
@Author: madrat
Asymmetric Ed25519 signing utilities for the Gatekeeper verifier.
"""

from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric import ed25519


@dataclass(slots=True)
class Ed25519Signer:
    """Manage an Ed25519 keypair used to sign verified binaries."""

    private_key_path: Path
    public_key_path: Path

    _private_key: Optional[ed25519.Ed25519PrivateKey] = None
    _public_key: Optional[ed25519.Ed25519PublicKey] = None

    def sign(self, payload: bytes) -> bytes:
        key = self._ensure_keypair()
        return key.sign(payload)

    def verify(self, payload: bytes, signature: bytes) -> bool:
        self._ensure_keypair()
        assert self._public_key is not None

        try:
            self._public_key.verify(signature, payload)
            return True
        except Exception:  # pragma: no cover - broad catch to simplify
            return False

    def public_material(self) -> bytes:
        self._ensure_keypair()
        assert self._public_key is not None
        return self._public_key.public_bytes(
            encoding=serialization.Encoding.Raw,
            format=serialization.PublicFormat.Raw,
        )

    # ------------------------------------------------------------------
    # Key management helpers
    # ------------------------------------------------------------------
    def _ensure_keypair(self) -> ed25519.Ed25519PrivateKey:
        if self._private_key is None:
            self._load_or_create()
        assert self._private_key is not None
        return self._private_key

    def _load_or_create(self) -> None:
        if self.private_key_path.exists() and self.public_key_path.exists():
            self._load()
            return

        self._generate()

    def _load(self) -> None:
        private_bytes = self.private_key_path.read_bytes()
        private_key = serialization.load_pem_private_key(private_bytes, password=None)
        if not isinstance(
            private_key, ed25519.Ed25519PrivateKey
        ):  # pragma: no cover - defensive
            raise TypeError("Stored private key is not an Ed25519 key")
        self._private_key = private_key

        public_bytes = self.public_key_path.read_bytes()
        public_key = serialization.load_pem_public_key(public_bytes)
        if not isinstance(
            public_key, ed25519.Ed25519PublicKey
        ):  # pragma: no cover - defensive
            raise TypeError("Stored public key is not an Ed25519 key")

        self._public_key = public_key

    def _generate(self) -> None:
        self.private_key_path.parent.mkdir(parents=True, exist_ok=True)
        self.public_key_path.parent.mkdir(parents=True, exist_ok=True)

        self._private_key = ed25519.Ed25519PrivateKey.generate()
        private_pem = self._private_key.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.NoEncryption(),
        )

        self._public_key = self._private_key.public_key()
        public_pem = self._public_key.public_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PublicFormat.SubjectPublicKeyInfo,
        )

        self.private_key_path.write_bytes(private_pem)
        self.public_key_path.write_bytes(public_pem)

        # self._restrict_permissions(self.private_key_path)

    # @staticmethod
    # def _restrict_permissions(path: Path) -> None:
    #     """Attempt to restrict file permissions on POSIX platforms."""

    #     try:
    #         os.chmod(path, 0o600)
    #     except PermissionError:  # pragma: no cover - platform dependent
    #         pass
