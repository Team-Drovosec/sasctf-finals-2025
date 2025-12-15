#!/usr/bin/env python3

import base64
import json
import logging
import os
import random
import subprocess
import sys
import traceback
from pathlib import Path
from typing import Any, Dict, Tuple

import aiohttp
import config
from aiohttp import client_exceptions, http_exceptions, web_exceptions
from generate_programs import init_generator
from gornilo import (
    CheckRequest,
    GetRequest,
    NewChecker,
    PutRequest,
    Verdict,
    VulnChecker,
)

basic_generator = init_generator()

PROJECT_PATH = Path(__file__).resolve().parent.parent
MAKE_DIR = PROJECT_PATH / "service" / "example_program"
CHECKER_ROOT = PROJECT_PATH / "checker"
CHECKER_GENERATED_PROGRAMS = PROJECT_PATH / "checker" / "generated_programs"
QEMU_TIMEOUT_SECONDS = 10
QEMU_EXECUTABLE = PROJECT_PATH / "service" / "executor" / "qemu-system-riscv64"
sys.path.insert(0, str(PROJECT_PATH))

from proggen.engine import ProgramGenerator
from proggen.patterns.base import PatternInfo
from service_client import GatekeeperClient
from sources.verifier.triton_verifier.symbolic_verifier import TritonVerifier

logging.getLogger("asyncio").setLevel(logging.WARNING)
logging.getLogger("urllib3.connectionpool").setLevel(logging.WARNING)
logging.getLogger("charset_normalizer").setLevel(logging.WARNING)
logging.getLogger("sources.verifier.triton_verifier.symbolic_verifier").setLevel(
    logging.WARNING
)
logging.getLogger("sources.verifier.triton_verifier.function_models").setLevel(
    logging.WARNING
)
logging.getLogger("sources.verifier.triton_verifier.memory_modeller").setLevel(
    logging.WARNING
)

checker = NewChecker()
program_generator = ProgramGenerator(root=CHECKER_ROOT, make_dir=MAKE_DIR)
global_seed = random.randrange(sys.maxsize)
rng = random.Random(global_seed)

logger = logging.getLogger(__name__)
logger.info("Starting verifier behavior tests with seed %d", global_seed)


DOWN_EXCEPTIONS = {
    client_exceptions.ServerConnectionError,
    client_exceptions.ClientConnectorError,
    client_exceptions.ServerTimeoutError,
    client_exceptions.ServerDisconnectedError,
    client_exceptions.ClientOSError,
    client_exceptions.ClientPayloadError,
}

MUMBLE_EXCEPTIONS = {
    http_exceptions.HttpProcessingError,
    client_exceptions.ClientResponseError,
    client_exceptions.ContentTypeError,
    client_exceptions.TooManyRedirects,
    json.decoder.JSONDecodeError,
    web_exceptions.HTTPClientError,
    http_exceptions.BadHttpMessage,
}

KNOWN_EXCEPTIONS = DOWN_EXCEPTIONS | MUMBLE_EXCEPTIONS


class ErrorChecker:
    def __init__(self):
        self.verdict = Verdict.OK()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, exc_traceback):
        if exc_type and any(issubclass(exc_type, e) for e in DOWN_EXCEPTIONS):
            self.verdict = Verdict.DOWN("Service is down")

        elif exc_type and any(issubclass(exc_type, e) for e in MUMBLE_EXCEPTIONS):
            self.verdict = Verdict.MUMBLE("Service is mumbled")

        elif exc_type == Verdict:
            self.verdict = exc_value

        if exc_type:
            print(exc_type, file=sys.stdout)
            print(exc_value, file=sys.stdout)
            traceback.print_tb(exc_traceback, file=sys.stdout)

            if exc_type not in KNOWN_EXCEPTIONS and exc_type is not Verdict:
                raise exc_value

        return True


# Check /healthz
async def do_test_health(client: GatekeeperClient, session: aiohttp.ClientSession):
    resp = await client.get_health(session)
    if resp != 200:
        raise Verdict.MUMBLE("Verifier health endpoint broken")


# Check /verify (missing program)
async def do_test_missing_program(
    client: GatekeeperClient, session: aiohttp.ClientSession
):
    resp = await client.verify_program(session, None)
    if not isinstance(resp, dict) or resp.get("error") != "missing program":
        raise Verdict.MUMBLE("Verifier wrong response for missing program (None)")

    resp = await client.verify_program(session, {})
    if not isinstance(resp, dict) or resp.get("error") != "missing program":
        raise Verdict.MUMBLE("Verifier wrong response for missing program ({})")


# Check /verify (invalid base64)
async def do_test_invalid_base64(
    client: GatekeeperClient, session: aiohttp.ClientSession
):
    invalid_b64_samples = [
        "abcd$%^&*()efgh",
        "12345 67890",
        "/////=====",
        "abcd\tefgh",
    ]

    payload = {"program": rng.choice(invalid_b64_samples)}
    resp = await client.verify_program(session, payload)
    if not isinstance(resp, dict) or resp.get("error") != "invalid base64 payload":
        raise Verdict.MUMBLE("Verifier wrong response for invalid base64 payload")


async def verifier_test_one(
    client: GatekeeperClient,
    session: aiohttp.ClientSession,
    program_path: Path,
    target_info: PatternInfo,
    mode: str = "reject",
) -> str | None:
    tv = TritonVerifier()
    program_bytes = program_path.read_bytes()
    report = tv.verify_program(program_bytes)

    if mode == "reject" and report.passed:
        logger.error(
            "Generated program for target '%s' was expected to be rejected, but passed verification, skipping test",
            target_info.name,
        )
        return

    if mode == "accept" and not report.passed:
        logger.error(
            "Generated program for target '%s' was expected to be accepted, but failed verification, skipping test",
            target_info.name,
        )
        return

    payload = {"program": base64.b64encode(program_bytes).decode()}
    resp = await client.verify_program(session, payload)

    expected_response = "rejected" if mode == "reject" else "success"
    if not isinstance(resp, dict) or resp.get("result") != expected_response:
        raise Verdict.MUMBLE(
            f"Verifier: wrong verification verdict for rejected program, expected {expected_response}, got {resp.get('result')}"
        )

    if not report.passed and resp.get("issue") != str(report):
        logger.error(
            "Verifier: wrong issue message for program, expected '%s', got '%s'",
            str(report),
            resp.get("issue"),
        )
        raise Verdict.MUMBLE(
            target_info.checker_message
            or f"Verifier: wrong issue message for rejected program, expected '{str(report)}', got '{resp.get('issue')}'"
        )

    if mode == "accept":
        return resp["signature"]


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

    logger.debug(f"QEMU exited with code {completed.returncode}")

    return completed.stdout


async def executor_test_one(
    verifier: GatekeeperClient,
    session: aiohttp.ClientSession,
    program_path: Path,
    target_info: PatternInfo,
):
    program_bytes = program_path.read_bytes()
    signature = await verifier_test_one(
        client=verifier,
        session=session,
        program_path=program_path,
        target_info=target_info,
        mode="accept",
    )

    golden_output_b64 = base64.b64encode(launch_qemu(program_path)).decode()

    payload = {
        "signature": signature,
        "program": base64.b64encode(program_bytes).decode(),
    }
    resp = await verifier.execute_program(session, payload)

    if not isinstance(resp, dict) or resp.get("status") != "success":
        logger.error(
            "Executor: wrong execution verdict for accepted program, expected 'success', got '%s', message: %s",
            resp.get("status"),
            resp.get("message"),
        )
        raise Verdict.MUMBLE(
            f"Executor: wrong execution verdict for accepted program, expected 'success', got '{resp.get('status')}'"
        )

    output_b64 = resp.get("stdout", "")
    if output_b64 != golden_output_b64:
        raise Verdict.MUMBLE(
            f"Executor: wrong program output for accepted program, expected '{golden_output_b64}', got '{output_b64}'"
        )


def generate_one(
    target: str, seed: int, overrides: Dict[str, Any] | None = None
) -> Tuple[Path, PatternInfo]:
    target_info = program_generator.get_target_info(target)
    gen_result = program_generator.generate(
        pattern=target,
        seed=seed,
        out_base=CHECKER_GENERATED_PROGRAMS,
        overrides=overrides,
    )

    logger.debug(
        "Generated program: '%s' for target '%s' with seed %d",
        gen_result.src_path,
        target_info.name,
        seed,
    )

    return gen_result.bin_path, target_info


async def do_test_verifier_behavior_rejects(
    verifier: GatekeeperClient, session: aiohttp.ClientSession
):
    """
    This function randomly samples around ~5 testcases to test the verifier behavior
    from the set of supported reject cases of the program generator.
    """
    modules = program_generator.list_modules_filter_by_category("reject")
    num_modules = min(7, len(modules))
    selected_modules = rng.sample(list(modules.keys()), k=num_modules)

    for module in selected_modules:
        targets = modules[module]
        if not targets:
            continue

        num_targets = rng.randint(1, min(3, len(targets)))
        targets = rng.sample(targets, k=num_targets)

        for target in targets:
            seed = rng.randint(0, sys.maxsize)
            path, target_info = generate_one(target, seed)
            await verifier_test_one(
                client=verifier,
                session=session,
                program_path=path,
                target_info=target_info,
                mode="reject",
            )


async def do_test_executor_behavior_accepts(
    verifier: GatekeeperClient, session: aiohttp.ClientSession
):
    """
    This function randomly samples around ~10 testcases to test the executor behavior
    """

    # A set of required tests that are always executed.
    # 1. verify symbolic return from `read_file_plain`
    # 2. check specifically for the one-provable-possible symbolic pc testcase

    modules = program_generator.list_modules_filter_by_category("accept")
    num_modules = min(10, len(modules))
    selected_modules = rng.sample(list(modules.keys()), k=num_modules)

    for module in selected_modules:
        targets = modules[module]
        if not targets:
            continue

        num_targets = rng.randint(1, min(3, len(targets)))
        targets = rng.choices(targets, k=num_targets)

        for target in targets:
            seed = rng.randint(0, sys.maxsize)
            program_path, target_info = generate_one(target, seed)
            await executor_test_one(
                verifier=verifier,
                session=session,
                program_path=program_path,
                target_info=target_info,
            )


async def do_test_executor_rejects_unsigned(
    verifier: GatekeeperClient, session: aiohttp.ClientSession
):
    """
    This function tests that the executor rejects unsigned programs.
    """

    random_signature = rng.randbytes(32).hex()
    random_program_size = rng.randint(1, 4096)
    random_program_bytes = rng.randbytes(random_program_size)
    payload = {
        "signature": random_signature,
        "program": base64.b64encode(random_program_bytes).decode(),
    }
    resp = await verifier.execute_program(session, payload)

    if not isinstance(resp, dict) or resp.get("status") != "error":
        raise Verdict.MUMBLE(
            f"Executor: wrong execution verdict for unsigned program, expected 'error', got '{resp.get('status')}'"
        )

    if resp.get("message") != "signature verification failed":
        raise Verdict.MUMBLE(
            f"Executor: wrong error message for unsigned program, expected 'signature verification failed', got '{resp.get('message')}'"
        )


@checker.define_check
async def check_service(request: CheckRequest) -> Verdict:
    with ErrorChecker() as ec:
        client = GatekeeperClient(request.hostname)
        async with aiohttp.ClientSession() as session:
            await do_test_health(client, session)
            await do_test_missing_program(client, session)
            await do_test_invalid_base64(client, session)
            await do_test_verifier_behavior_rejects(client, session)
            await do_test_executor_rejects_unsigned(client, session)
            await do_test_executor_behavior_accepts(client, session)

        ec.verdict = Verdict.OK("Verifier is up and working properly")
    return ec.verdict


@checker.define_vuln("Plain test")
class PlainChecker(VulnChecker):
    @staticmethod
    def put(request: PutRequest) -> Verdict:
        with ErrorChecker() as ec:
            client = GatekeeperClient(request.hostname, config.GATEKEEPER_PORT)
            b_flag = request.flag.encode()
            prog = base64.b64encode(
                basic_generator.get_storage("put_plain", flag=b_flag)
            ).decode()

            verify_response = client.verify_program_sync({"program": prog})
            if (
                verify_response.get("result") == ""
                or verify_response.get("result") != "success"
            ):
                raise Verdict.MUMBLE(
                    "Verify: incorrect verify status for correct program"
                )

            signature = verify_response.get("signature")

            execute_response = client.execute_program_sync(
                {"program": prog, "signature": signature}
            )

            if (
                execute_response.get("status") == ""
                or execute_response.get("status") != "success"
            ):
                raise Verdict.MUMBLE(
                    "Execute: incorrect execute status for correct program"
                )

            stdout = base64.b64decode(execute_response.get("stdout"))
            if len(stdout) == 0:
                raise Verdict.MUMBLE("Execute: incorrect output")

            try:
                handle = int(stdout.decode(), 16)
            except ValueError:
                raise Verdict.MUMBLE("Execute: incorrect output")

            flag_id = json.dumps({"handle": handle})

            ec.verdict = Verdict.OK_WITH_FLAG_ID(handle & 0xFF000000000000FF, flag_id)

        return ec.verdict
        # return Verdict.OK_WITH_FLAG_ID("dummy_id", f"Stored flag: {request.flag}")

    @staticmethod
    def get(request: GetRequest) -> Verdict:
        with ErrorChecker() as ec:
            client = GatekeeperClient(request.hostname, config.GATEKEEPER_PORT)
            res_json = json.loads(request.flag_id)
            handle = res_json["handle"]

            key = rng.getrandbits(64)
            prog = program_generator.generate(
                pattern="get_plain_1",
                seed=handle,
                out_base=CHECKER_GENERATED_PROGRAMS,
                overrides={"enc_handle": handle ^ key, "enc_key": key},
            )
            prog_b64 = base64.b64encode(prog.bin_path.read_bytes()).decode()

            verify_response = client.verify_program_sync({"program": prog_b64})
            if (
                verify_response.get("result") == ""
                or verify_response.get("result") != "success"
            ):
                logger.error("Verify response: %s", verify_response)
                raise Verdict.MUMBLE(
                    "Verify: incorrect verify status for correct program"
                )

            signature = verify_response.get("signature")

            execute_response = client.execute_program_sync(
                {"program": prog_b64, "signature": signature}
            )

            if (
                execute_response.get("status") == ""
                or execute_response.get("status") != "success"
            ):
                raise Verdict.MUMBLE(
                    "Execute: incorrect execute status for correct program"
                )

            stdout = base64.b64decode(execute_response.get("stdout"))
            if request.flag.encode() not in stdout:
                logger.error(
                    "Stdout not matching. Expected: %s, got: %s",
                    request.flag.encode(),
                    stdout,
                )
                raise Verdict.MUMBLE("Execute: incorrect output")

            ec.verdict = Verdict.OK(
                "Flag retrieved successfully. " + f"Retrieved flag: {request.flag}"
            )

        return ec.verdict


@checker.define_vuln("encrypted test")
class EncryptedChecker(VulnChecker):
    @staticmethod
    def put(request: PutRequest) -> Verdict:
        with ErrorChecker() as ec:
            client = GatekeeperClient(request.hostname, config.GATEKEEPER_PORT)
            b_flag = request.flag.encode()

            iv = os.urandom(12)
            prog = base64.b64encode(
                basic_generator.get_storage("put_enc", flag=b_flag, iv=iv)
            ).decode()

            verify_response = client.verify_program_sync({"program": prog})
            if (
                verify_response.get("result") == ""
                or verify_response.get("result") != "success"
            ):
                raise Verdict.MUMBLE(
                    "Verify: incorrect verify status for correct program"
                )

            signature = verify_response.get("signature")
            execute_response = client.execute_program_sync(
                {"program": prog, "signature": signature}
            )

            if (
                execute_response.get("status") == ""
                or execute_response.get("status") != "success"
            ):
                raise Verdict.MUMBLE(
                    "Execute: incorrect execute status for correct program"
                )

            stdout = base64.b64decode(execute_response.get("stdout"))
            if len(stdout) == 0:
                raise Verdict.MUMBLE("Execute: incorrect output")

            try:
                handle, tag = stdout.split(b"\n", 1)
                handle = int(handle.decode(), 16)

            except Exception:
                raise Verdict.MUMBLE("Execute: incorrect output")

            flag_id = json.dumps(
                {"handle": handle, "tag": base64.b64encode(tag).decode()}
            )

            ec.verdict = Verdict.OK_WITH_FLAG_ID(handle & 0xFF000000000000FF, flag_id)

        return ec.verdict
        # return Verdict.OK_WITH_FLAG_ID("dummy_id", f"Stored flag: {request.flag}")

    @staticmethod
    def get(request: GetRequest) -> Verdict:
        with ErrorChecker() as ec:
            client = GatekeeperClient(request.hostname, config.GATEKEEPER_PORT)
            res_json = json.loads(request.flag_id)
            handle = res_json["handle"]

            tag = res_json["tag"]
            prog = base64.b64encode(
                basic_generator.get_storage(
                    "get_enc",
                    handle=handle.to_bytes(8, "little"),
                    tag=base64.b64decode(tag),
                )
            ).decode()

            verify_response = client.verify_program_sync({"program": prog})
            if (
                verify_response.get("result") == ""
                or verify_response.get("result") != "success"
            ):
                raise Verdict.MUMBLE(
                    "Verify: incorrect verify status for correct program"
                )

            signature = verify_response.get("signature")

            execute_response = client.execute_program_sync(
                {"program": prog, "signature": signature}
            )

            if (
                execute_response.get("status") == ""
                or execute_response.get("status") != "success"
            ):
                raise Verdict.MUMBLE(
                    "Execute: incorrect execute status for correct program"
                )

            stdout = base64.b64decode(execute_response.get("stdout"))
            if stdout != request.flag.encode():
                raise Verdict.MUMBLE("Execute: incorrect output")

            ec.verdict = Verdict.OK(
                "Flag retrieved successfully. " + f"Retrieved flag: {request.flag}"
            )

        return ec.verdict


if __name__ == "__main__":
    checker.run()
