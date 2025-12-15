#!/usr/bin/env python3
"""
Orchestrator service that verifies programs with an external verifier service,
checks Ed25519 signatures, and runs qemu instances as subprocesses.

Endpoint:
    POST /execute
    Request:
    {
      "signature": "<base64 signature>",
      "program": "<base64 program bytes>"
    }

    Response:
    {
      "status": "<status string>",
      "stdout": "<base64-encoded stdout>"
      "message": "<error message>"
    }
"""

from __future__ import annotations

import base64
import logging
import os
import subprocess
import tempfile
import threading
import traceback
from concurrent.futures import Future, ThreadPoolExecutor
from dataclasses import dataclass
from pathlib import Path
from typing import Tuple

import requests
from cryptography.hazmat.primitives.asymmetric import ed25519
from flask import Flask, jsonify, request

logging.basicConfig(
    level=logging.DEBUG,
    format="[%(asctime)s] (%(levelname)s) %(name)s: %(message)s",
    datefmt="%H:%M:%S",
)
logger = logging.getLogger(__name__)

# Create encryption keys at: `/var/gatekeeper/executor/enc_key.bin` if not present
enc_key_path = Path("/var/gatekeeper/executor/enc_key.bin")
if not enc_key_path.exists():
    enc_key = os.urandom(16)
    enc_key_path.parent.mkdir(parents=True, exist_ok=True)
    with open(enc_key_path, "wb") as f:
        f.write(enc_key)
    logger.info(f"Generated new encryption key at {enc_key_path}")

# ---------------------------
# Configuration constants
# ---------------------------
VERIFIER_BASE_URL = os.getenv("VERIFIER_BASE_URL", "http://127.0.0.1:2046/")
PUBLIC_KEY_ENDPOINT = f"{VERIFIER_BASE_URL}/public-key"

# The qemu executable to run
QEMU_EXECUTABLE = "/opt/gatekeeper-executor/qemu-system-riscv64"

# Timeout for qemu subprocess (seconds)
QEMU_TIMEOUT_SECONDS = 10

# Maximum concurrent qemu instances managed by this orchestrator
MAX_QEMU_INSTANCES = 8


@dataclass(slots=True)
class Ed25519Signer:
    """
    Simplified Ed25519 verifier that uses only a public key file.
    """

    _public_key_bytes: bytes

    def verify(self, payload: bytes, signature: bytes) -> bool:
        """Return True if signature is valid for payload, False otherwise."""
        public_key = ed25519.Ed25519PublicKey.from_public_bytes(self._public_key_bytes)

        try:
            public_key.verify(signature, payload)
            return True
        except Exception as exc:
            print(exc)
            return False


def get_verifier_public_key(timeout: int = 10) -> bytes:
    """
    Fetch the public key from verifier's /public-key endpoint.
    """
    resp = requests.get(PUBLIC_KEY_ENDPOINT, timeout=timeout)
    resp.raise_for_status()
    return bytes.fromhex(resp.json()["public_key"])


def write_bytes_to_tempfile(
    data: bytes, suffix: str = "", directory: str = "/var/gatekeeper/executor/programs"
) -> Path:
    """Write bytes to a temp file and return its Path. File will be deleted on program exit when possible."""
    programs_dir = Path(directory)
    programs_dir.mkdir(parents=True, exist_ok=True)

    tf = tempfile.NamedTemporaryFile(delete=False, suffix=suffix, dir=str(programs_dir))
    tf.write(data)
    tf.flush()
    tf.close()
    return Path(tf.name)


def launch_qemu(bios_path: Path, timeout: int = QEMU_TIMEOUT_SECONDS) -> bytes:
    """
    Launch qemu subprocess with command:
        qemu -machine gatekeeper -bios <binary>
    Returns the stdout bytes produced by qemu (or raises subprocess.TimeoutExpired).
    """
    cmd = [QEMU_EXECUTABLE, "-machine", "gatekeeper", "-bios", str(bios_path)]
    completed = subprocess.run(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=timeout
    )
    return completed.stdout


# ---------------------------
# QEMU orchestration (concurrency)
# ---------------------------
_executor = ThreadPoolExecutor(max_workers=MAX_QEMU_INSTANCES)
_executor_lock = threading.Lock()


def submit_qemu_and_wait(
    bios_path: Path, timeout: int = QEMU_TIMEOUT_SECONDS
) -> Tuple[bool, str]:
    """
    Submit a qemu launch to the internal executor and wait for completion.
    Returns (success, stdout_string_or_error_message)
    """

    def _task() -> bytes:
        return launch_qemu(bios_path, timeout=timeout)

    with _executor_lock:
        future: Future = _executor.submit(_task)

    try:
        stdout_bytes: bytes = future.result(timeout=timeout + 5)  # small buffer
        stdout_b64 = base64.b64encode(stdout_bytes).decode("ascii")
        return True, stdout_b64
    except Exception as exc:
        tb = traceback.format_exc()
        logger.error(f"qemu execution failed: {exc}\n{tb}")
        return False, f"qemu execution failed: {exc}"


# ---------------------------
# Flask web service
# ---------------------------
app = Flask(__name__)


@app.route("/execute", methods=["POST"])
def execute():
    """
    Execute endpoint:
      - expects JSON with "signature" (string, base64) and "program" (base64)
      - fetch verifier public key, and verify signature with Ed25519Signer
      - if ok: write program to temp file, run qemu subprocess, return stdout (base64-encoded)
    """
    try:
        body = request.get_json(force=True)
        if not isinstance(body, dict):
            return (
                jsonify({"status": "error", "stdout": "", "message": "invalid JSON"}),
                400,
            )
        if (
            "program" not in body
            or "signature" not in body
            or not body["program"]
            or not body["signature"]
        ):
            return (
                jsonify(
                    {
                        "status": "error",
                        "stdout": "",
                        "message": "missing 'program' or 'signature'",
                    }
                ),
                400,
            )

        provided_sig_bytes = bytes.fromhex(body["signature"])
        program_b64 = body["program"]

        # decode inputs
        try:
            program_bytes = base64.b64decode(program_b64)
        except Exception:
            return (
                jsonify(
                    {
                        "status": "error",
                        "stdout": "",
                        "message": "invalid base64 program",
                    }
                ),
                400,
            )

        # fetch public key from verifier
        try:
            pubkey_bytes = get_verifier_public_key()
        except Exception as exc:
            return (
                jsonify(
                    {
                        "status": "error",
                        "stdout": "",
                        "message": f"failed to fetch public key: {exc}",
                    }
                ),
                502,
            )

        signer = Ed25519Signer(pubkey_bytes)
        try:
            ok = signer.verify(program_bytes, provided_sig_bytes)
        except Exception as exc:
            return (
                jsonify(
                    {
                        "status": "error",
                        "stdout": "",
                        "message": f"public key verification failed: {exc}",
                    }
                ),
                400,
            )

        if not ok:
            return (
                jsonify(
                    {
                        "status": "error",
                        "stdout": "",
                        "message": "signature verification failed",
                    }
                ),
                400,
            )

        # signature checked - write program to temp file for qemu BIOS and launch qemu
        bios_path = write_bytes_to_tempfile(program_bytes, suffix=".bin")
        success, out = submit_qemu_and_wait(bios_path, timeout=QEMU_TIMEOUT_SECONDS)

        # always return stdout base64 to be safe (user sample shows base64-like)
        if success:
            return jsonify({"status": "success", "stdout": out}), 200
        else:
            # out contains error message
            return jsonify({"status": "error", "stdout": "", "message": out}), 500

    except Exception as exc:
        tb = traceback.format_exc()
        logger.error("Internal Error Occurred: %s", tb)
        return (
            jsonify(
                {
                    "status": "error",
                    "stdout": "",
                    "message": f"internal error: {exc}",
                }
            ),
            500,
        )


if __name__ == "__main__":
    # start Flask
    app.run(host="0.0.0.0", port=2047, threaded=True)
