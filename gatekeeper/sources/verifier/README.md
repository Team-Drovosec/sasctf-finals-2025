# Gatekeeper Verifier Service

Prototype service scaffolding for the Gatekeeper verifier. The Flask API exposes
endpoints for signing trusted binaries, and returning the Ed25519 public key.

## Quickstart

Create and activate python3.12 virtualenv:

```bash
python3.12 -m venv ~/venvs/py312
source ~/venvs/py312/bin/activate
pip install -r requirements.txt
```

Running standalone verification on a binary:

```bash
pushd .; cd ../../service/example_program ; make && make disasm ; popd ; time python -m triton_verifier.main ../../service/example_program/build/example.bin
```

Profiling the verification process:

```bash
python -m cProfile -o out.prof -m triton_verifier.main ../../service/example_program/build/example.bin
snakeviz out.prof
```

```bash
# Build the container image
docker build -t gatekeeper-verifier .

# Run the service locally
docker run --rm -p 8080:8080 gatekeeper-verifier
```

Use `VERIFIER_KEY_DIR`, `VERIFIER_PRIVATE_KEY_PATH`, or `VERIFIER_PUBLIC_KEY_PATH` to override the location of the auto-generated Ed25519 keypair.

## Running the tests

Install the Python dependencies and invoke `pytest` from the verifier root:

```bash
pip install -r requirements.txt pytest
pytest
```

## API Endpoints

| Method | Path          | Description                                      |
| ------ | ------------- | ------------------------------------------------ |
| POST   | `/verify`     | Validates the program and returns its signature. |
| GET    | `/public-key` | Returns the Ed25519 public key (intentional leak). |
| GET    | `/healthz`    | Liveness probe.                                 |
