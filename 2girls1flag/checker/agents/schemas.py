from pydantic import BaseModel, Field


class QuestionRequest(BaseModel):
    context: str = Field(..., description="facts about the girl that you can use to formulate a question, but you are obliged not to reveal the facts themselves, especially if the question is directly related to them.")
    query: str = Field(..., description="the goal/topic for the question")


class VerifyRequest(BaseModel):
    context: str = Field(..., description="situation, prior chat, her stated facts/interests, constraints, tone/language rules")
    query: str = Field(..., description="the goal/topic the user asked about")
    user_message: str = Field(..., description="the user's message sent to the girl")
    girl_answer: str = Field(..., description="the girl's reply to validate")


class VerifyResponse(BaseModel):
    is_verified: bool = Field(..., description="true only if all criteria pass; else false.")
    reason: str = Field(..., description="explanation of the result (short, one sentence)")
