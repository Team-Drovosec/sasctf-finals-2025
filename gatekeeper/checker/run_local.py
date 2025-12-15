import asyncio
from checker import check_service
from gornilo import CheckRequest

async def main():
    request = CheckRequest(hostname="127.0.0.1")
    verdict = await check_service(request)
    print(verdict)

if __name__ == "__main__":
    asyncio.run(main())