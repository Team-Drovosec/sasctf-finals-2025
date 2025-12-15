"""Flask application factory for the Gatekeeper verifier."""

from __future__ import annotations

import logging
from flask import Flask

from .config import AppConfig
from .routes import register_routes

logger = logging.getLogger(__name__)


def create_app(config: type[AppConfig] | None = None) -> Flask:
    """Create and configure the Flask application instance."""

    app = Flask(__name__)
    app.config.from_object(config or AppConfig())

    register_routes(app)

    logger.info("Verifier service initialised")
    return app
