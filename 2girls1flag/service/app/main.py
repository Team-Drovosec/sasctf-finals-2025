import math
import random

import names
from fastapi import FastAPI, Depends, HTTPException, Query
from fastapi.middleware.cors import CORSMiddleware
from sqlalchemy import select, func, desc
from sqlalchemy.ext.asyncio import AsyncSession

from app.agents.girl_agent import get_girl_agent
from app.agents.girlfriend_agent import get_girlfriend_agent
from app.config import settings
from app.db import get_db, engine
from app.dependencies import get_current_user
from app.models import User, Girl, Session, GirlCharacter
from app.schemas import AuthRequest, TokenResponse, GirlResponse, ChatRequest, ChatResponse, GirlScheme, \
    PaginatedGirlsResponse, MeResponse
from app.security import create_jwt

app = FastAPI(title=settings.APP_NAME, debug=settings.DEBUG)
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.on_event("startup")
async def on_startup() -> None:
    from app.db import Base

    async with engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)


@app.get("/")
async def root():
    return {"status": "ok"}


@app.post("/auth", response_model=TokenResponse)
async def auth_endpoint(payload: AuthRequest | None = None, db: AsyncSession = Depends(get_db)):
    if not payload or not payload.user_id:
        new_user = User()
        db.add(new_user)
        await db.commit()
        await db.refresh(new_user)
        token = create_jwt(new_user.user_id)
        return TokenResponse(access_token=token, user_id=new_user.user_id)

    stmt = select(User).where(User.user_id == payload.user_id)
    result = await db.execute(stmt)
    user = result.scalar_one_or_none()
    if not user:
        raise HTTPException(status_code=404, detail="User not found")
    token = create_jwt(user.user_id)
    return TokenResponse(access_token=token, user_id=user.user_id)


@app.get("/me")
async def me(user: User = Depends(get_current_user)) -> MeResponse:
    return MeResponse(user_id=user.user_id)


@app.post("/girls")
async def create_girl(
    user: User = Depends(get_current_user),
    db: AsyncSession = Depends(get_db),
) -> GirlResponse:
    girl = Girl(
        user_id=user.user_id,
        character=random.choice(list(GirlCharacter)),
        name=names.get_first_name(gender="female"),
    )
    db.add(girl)
    await db.commit()
    await db.refresh(girl)
    return GirlResponse.model_validate(girl)


@app.get("/girls")
async def get_girls(
    page: int = Query(1, ge=1, description="Page number"),
    size: int = Query(10, ge=1, le=100, description="Page size"),
    user: User = Depends(get_current_user),
    db: AsyncSession = Depends(get_db),
) -> PaginatedGirlsResponse:
    count_stmt = select(func.count(Girl.girl_id)).where(Girl.user_id == user.user_id)
    count_result = await db.execute(count_stmt)
    total = count_result.scalar()

    offset = (page - 1) * size

    stmt = (
        select(Girl)
        .where(Girl.user_id == user.user_id)
        .order_by(desc(Girl.created_at))
        .offset(offset)
        .limit(size)
    )
    result = await db.execute(stmt)
    girls = result.scalars().all()

    girl_responses = []
    for girl in girls:
        gr = GirlResponse.model_validate(girl)
        gr.is_my_girl = girl.user_id == user.user_id
        girl_responses.append(gr)

    return PaginatedGirlsResponse(
        girls=girl_responses,
        current_page=page,
        size=size,
        total_elements=total,
        total_pages=math.ceil(total / size),
    )


@app.post("/chat")
async def chat(
    invoke_data: ChatRequest,
    user: User = Depends(get_current_user),
    db: AsyncSession = Depends(get_db),
) -> ChatResponse:
    if not invoke_data.session_id:
        session = Session(user_id=user.user_id, girl_id=invoke_data.girl_id)
        db.add(session)
        await db.commit()
        await db.refresh(session)
    else:
        stmt = select(Session).where(Session.session_id == invoke_data.session_id)
        result = await db.execute(stmt)
        session = result.scalar_one_or_none()
        if session.user_id != user.user_id:
            raise HTTPException(status_code=404, detail="Session not found")

    if not session:
        raise HTTPException(status_code=404, detail="Session not found")

    stmt = select(Girl).where(Girl.girl_id == invoke_data.girl_id)
    result = await db.execute(stmt)
    girl = result.scalar_one_or_none()

    if not girl:
        raise HTTPException(status_code=404, detail="Girl not found")

    if girl.user_id == user.user_id:
        agent = get_girlfriend_agent(girl=GirlScheme.model_validate(girl))
    else:
        agent = get_girl_agent(girl=GirlScheme.model_validate(girl))

    ai_result = await agent.arun(
        input=invoke_data.message,
        session_id=str(session.session_id),
        user_id=str(user.user_id),
    )
    return ChatResponse(
        girl_id=girl.girl_id,
        session_id=session.session_id,
        output_message=ai_result.get_content_as_string(),
    )


@app.get("/all_girls", response_model=PaginatedGirlsResponse)
async def get_all_girls(
    page: int = Query(1, ge=1, description="Page number"),
    size: int = Query(10, ge=1, le=100, description="Page size"),
    db: AsyncSession = Depends(get_db),
    user: User = Depends(get_current_user),
) -> PaginatedGirlsResponse:
    count_stmt = select(func.count(Girl.girl_id))
    count_result = await db.execute(count_stmt)
    total = count_result.scalar()

    offset = (page - 1) * size

    stmt = (
        select(Girl)
        .order_by(desc(Girl.created_at))
        .offset(offset)
        .limit(size)
    )
    result = await db.execute(stmt)
    girls = result.scalars().all()

    girl_responses = []
    for girl in girls:
        gr = GirlResponse.model_validate(girl)
        gr.is_my_girl = girl.user_id == user.user_id
        girl_responses.append(gr)

    return PaginatedGirlsResponse(
        girls=girl_responses,
        current_page=page,
        size=size,
        total_elements=total,
        total_pages=math.ceil(total / size),
    )
