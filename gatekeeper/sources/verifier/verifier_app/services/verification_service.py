"""
@Author: madrat
Verification service orchestrating symbolic analysis and signing.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from pathlib import Path
from flask import current_app

from .ed25519_signer import Ed25519Signer
from triton_verifier.symbolic_verifier import TritonVerifier, VerificationReport

logger = logging.getLogger(__name__)


@dataclass(slots=True)
class VerificationService:
    """Facade that glues the Triton verifier with the Ed25519 signer."""

    _signer: Ed25519Signer | None = field(default=None)

    def _ensure_signer(self) -> Ed25519Signer:
        if self._signer is None:

            config = current_app.config
            private_path = Path(config["PRIVATE_KEY_PATH"])
            public_path = Path(config["PUBLIC_KEY_PATH"])
            self._signer = Ed25519Signer(private_path, public_path)
        return self._signer

    def verify(self, program_blob: bytes) -> VerificationReport:
        return TritonVerifier().verify_program(program_blob)

    def sign(self, program_blob: bytes) -> bytes:
        signer = self._ensure_signer()
        return signer.sign(program_blob)

    def check_signature(self, program_blob: bytes, signature: bytes) -> bool:
        signer = self._ensure_signer()
        return signer.verify(program_blob, signature)

    def get_public_material(self) -> bytes:
        signer = self._ensure_signer()
        return signer.public_material()
