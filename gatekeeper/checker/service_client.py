#!/usr/bin/env python3

import aiohttp
import asyncio
import base64
from config import HTTP_TIMEOUT, GATEKEEPER_PORT


class GatekeeperClient:
    def __init__(self, host: str, port: int = GATEKEEPER_PORT):
        self.base_url = f"http://{host}:{port}"
        self.timeout = HTTP_TIMEOUT

    async def get_health(self, session: aiohttp.ClientSession):
        url = f"{self.base_url}/healthz"
        async with session.get(url, timeout=self.timeout) as response:
            return response.status

    def get_health_sync(self) -> int:
        async def _get_health():
            async with aiohttp.ClientSession() as session:
                return await self.get_health(session)

        return asyncio.run(_get_health())

    async def verify_program(
        self, session: aiohttp.ClientSession, payload: dict | None
    ):
        url = f"{self.base_url}/verify"
        async with session.post(url, json=payload, timeout=self.timeout) as response:
            return await response.json()

    def verify_program_sync(self, payload: dict | None):
        async def _verify_program():
            async with aiohttp.ClientSession() as session:
                return await self.verify_program(session, payload)

        return asyncio.run(_verify_program())

    async def execute_program(
        self, session: aiohttp.ClientSession, payload: dict | None
    ):
        url = f"{self.base_url}/execute"
        async with session.post(url, json=payload, timeout=self.timeout) as response:
            return await response.json()

    def execute_program_sync(self, payload: dict | None):
        async def _execute_program():
            async with aiohttp.ClientSession() as session:
                return await self.execute_program(session, payload)

        return asyncio.run(_execute_program())


async def main():
    verifier = GatekeeperClient("localhost")

    async with aiohttp.ClientSession() as session:
        health = await verifier.get_health(session)
        print("Healthcheck:          ", health)

        program = base64.b64encode(
            bytes.fromhex("130101ff23341100ef004000730010006f000000")
        ).decode()
        verify_result = await verifier.verify_program(session, {"program": program})
        print("Verification result:  ", verify_result)

        execute_result = await verifier.execute_program(
            session, {"signature": verify_result["signature"], "program": program}
        )
        print("Execution result:     ", execute_result)


if __name__ == "__main__":
    asyncio.run(main())
