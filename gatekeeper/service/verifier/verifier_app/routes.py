"""HTTP route handlers for the verifier service."""

from __future__ import annotations

import base64
import logging
from http import HTTPStatus

from flask import Blueprint, Flask, jsonify, request

from .services.verification_service import VerificationService

logger = logging.getLogger(__name__)


def register_routes(app: Flask) -> None:
    """Attach application routes to the given Flask application."""

    api = Blueprint("verifier_api", __name__)
    verifier = VerificationService()

    @api.route("/verify", methods=["POST"])
    def verify_program():
        payload = request.get_json(silent=True) or {}
        program_b64 = payload.get("program")

        if not program_b64:
            return (
                jsonify({"result": "error", "error": "missing program"}),
                HTTPStatus.BAD_REQUEST,
            )

        try:
            program_bytes = base64.b64decode(program_b64, validate=True)
        except (
            ValueError,
            TypeError,
        ) as exc:  # pragma: no cover - simple input validation
            logger.debug("Failed to base64 decode payload: %s", exc)
            return (
                jsonify({"result": "error", "error": "invalid base64 payload"}),
                HTTPStatus.BAD_REQUEST,
            )

        report = verifier.verify(program_bytes)
        if not report.passed:
            logger.info("Program verification failed: %s", report.issue)
            return (
                jsonify({"result": "rejected", "issue": str(report)}),
                HTTPStatus.UNPROCESSABLE_ENTITY,
            )

        signature = verifier.sign(program_bytes)
        logger.info(
            "Program verified and signed successfully, signature generated: %s",
            signature.hex(),
        )
        return (
            jsonify(
                {
                    "result": "success",
                    "signature": signature.hex(),
                }
            ),
            HTTPStatus.OK,
        )

    @api.route("/public-key", methods=["GET"])
    def get_public_key():
        return (
            jsonify(
                {
                    "result": "success",
                    "public_key": verifier.get_public_material().hex(),
                }
            ),
            HTTPStatus.OK,
        )

    @api.route("/healthz", methods=["GET"])
    def healthcheck():
        return jsonify({"status": "ok"}), HTTPStatus.OK

    app.register_blueprint(api)
