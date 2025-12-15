import base64

from cryptography.hazmat.primitives.asymmetric import ed25519

from ..triton_verifier.symbolic_verifier import VerificationReport


def test_verify_endpoint_requires_program(client):
    response = client.post("/verify", json={})

    assert response.status_code == 400
    assert response.get_json()["result"] == "error"


def test_verify_endpoint_returns_signature(client, monkeypatch):
    def fake_verify(_self, _program_blob):
        return VerificationReport(passed=True)

    monkeypatch.setattr(
        "verifier_app.services.verification_service.TritonVerifier.verify_program",
        fake_verify,
    )

    payload = base64.b64encode(b"test-program").decode()
    response = client.post("/verify", json={"program": payload})

    assert response.status_code == 200
    body = response.get_json()
    signature_bytes = bytes.fromhex(body["signature"])
    assert len(signature_bytes) == 64

    public_resp = client.get("/public-key")
    public_bytes = bytes.fromhex(public_resp.get_json()["public_key"])

    ed25519.Ed25519PublicKey.from_public_bytes(public_bytes).verify(
        signature_bytes,
        b"test-program",
    )


def test_verify_endpoint_reports_issue(client, monkeypatch):
    def fake_verify(_self, _program_blob):
        return VerificationReport(passed=False, issue="boom")

    monkeypatch.setattr(
        "verifier_app.services.verification_service.TritonVerifier.verify_program",
        fake_verify,
    )

    payload = base64.b64encode(b"bad").decode()
    response = client.post("/verify", json={"program": payload})

    assert response.status_code == 422
    body = response.get_json()
    assert body["result"] == "rejected"
    assert body["issue"] == "boom"


def test_public_key_endpoint_returns_key(client):
    response = client.get("/public-key")

    assert response.status_code == 200
    body = response.get_json()
    pubkey = bytes.fromhex(body["public_key"])
    assert len(pubkey) == 32


def test_healthcheck(client):
    response = client.get("/healthz")

    assert response.status_code == 200
    assert response.get_json()["status"] == "ok"
