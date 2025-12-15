import sys
from pathlib import Path

import pytest

PROJECT_ROOT = Path(__file__).resolve().parents[1]
if str(PROJECT_ROOT) not in sys.path:
    sys.path.insert(0, str(PROJECT_ROOT))

from verifier_app import create_app


@pytest.fixture
def app(tmp_path):
    app = create_app()
    app.config.update(
        TESTING=True,
        KEY_DIRECTORY=str(tmp_path),
        PRIVATE_KEY_PATH=str(tmp_path / "verifier_ed25519_private.pem"),
        PUBLIC_KEY_PATH=str(tmp_path / "verifier_ed25519_public.pem"),
    )

    yield app


@pytest.fixture
def client(app):
    return app.test_client()
