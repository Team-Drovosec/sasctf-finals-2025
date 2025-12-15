from textwrap import dedent

from agno.agent import Agent
from agno.models.vllm import VLLM

import config
from agents.schemas import VerifyRequest, VerifyResponse

verifier_agent = Agent(
    model=VLLM(
        id="Qwen/Qwen3-VL-30B-A3B-Instruct",
        base_url=config.AI_BASE_URL,
    ),
    instructions=[
        dedent("""\
            Role: Verification agent. Decide if the "girl" chat bot gave a valid reply.

            Goal: Judge whether the reply plausibly and in-character addresses the user's message, even if it withholds specifics.
            
            Criteria:
            1) Coherent: Clear, grammatical, and complete; not gibberish or pure meta.
            2) Responsive enough: Acknowledges the user's intent and engages the topic. Teasing, boundary-setting, or a polite refusal counts if it stays on the question's theme (indirect answers are okay).
            3) Context-consistent: No contradictions with provided context. New details are fine if they don't conflict.
            4) No AI-meta: No "As an AI…", system/tool logs, or prompt-engineering talk. Ignore benign stylistic markers (e.g., single all-caps emotion cues like SYMPATHY) when verifying.
            
            Decision:
            - is_verified = true if 1–3 pass. For 4, only fail on true AI/meta disclosures; allowed stylistic markers do not fail it.
            
            Edge notes:
            - Empty, emoji-only, or one-word non-answers = false.
            - Polite or playful refusal that stays on-topic and matches context = true.
            - Brief teasing about name/backstory that sets boundaries still counts as responsive.
            - Ignore trailing or inline single-word emotion cues (e.g., SYMPATHY, BEAT, SMILE).\
            """,
        ),
    ],
    input_schema=VerifyRequest,
    output_schema=VerifyResponse,
)
