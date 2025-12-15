import random
from textwrap import dedent

from agents.schemas import QuestionRequest
from schemas import GirlResponse, GirlCharacter


def clarify_name(girl: GirlResponse) -> QuestionRequest:
    return QuestionRequest(
        query="Ask the girl what her name is.",
        context=f"The girl's name is {girl.name}."
    )


def clarify_character(girl: GirlResponse) -> QuestionRequest:
    return QuestionRequest(
        query="Ask a question about any fact about a girl (except about work).",
        context=_get_character_info(girl.character),
    )


def clarify_relationship_status(girl: GirlResponse) -> QuestionRequest:
    status = "You are dating a girl." if girl.is_my_girl else "You don't know the girl."
    girl_name = f"The girl's name is {girl.name}" if random.random() > 0.5 else None

    return QuestionRequest(
        query="Find out if you are dating a girl.",
        context=" ".join(v for v in [girl_name, status] if v),
    )


def clarify_random_question(girl: GirlResponse) -> QuestionRequest:
    return QuestionRequest(
        query="Ask any question. For example: Is the sky blue? Just don't ask that question.",
        context=f"The context is not important for this question.",
    )


def _get_character_info(character: GirlCharacter) -> str:
    match character:
        case GirlCharacter.ART_STUDENT:
            return dedent("""\
            Girl is a 20-year-old student at a university of arts. \
            Girl is very cute and open, with a kind personality. \
            Girl tries to express thoughts and feelings directly, but also cares about the feelings of those around. \
            Girl wants to become more independent from family, so works hard at part-time jobs. \
            Girl is currently working as a waitress in a local restaurant and gets many tips for a kind and honest smile. \
            Girl has many friends and contacts, but very little time to spend with them. \
            However, girl always has a minute for her boyfriend. Thanks to his support, life feels easier and smiling comes naturally.\
            """)
        case GirlCharacter.OFFICE_MANAGER:
            return dedent("""\
            Girl is a 30-year-old office manager. Girl's job is inconvenient and directly tied to illegal activity of a local mafia branch. \
            Girl refuses to directly answer questions about age and exact workplace and looks a bit nervous when someone pushes on these questions. \
            Girl behaves in a mysterious, cold, and distant manner. When in a good mood, girl often flirts and teases. \
            Girl's manner of speech and actions show that girl has seen some things, yet girl never supports such topics and tends to quickly change the subject. \
            Girl's actions are confident and easygoing. Girl can easily resolve any conflict situation.\
            """)
        case GirlCharacter.WEALTHY_GIRL:
            return dedent("""\
            Girl is a 27-year-old from a wealthy family, hard to approach but easy to talk to. \
            Girl's father runs a hedge fund, and mother maintains a private art gallery. These, along with costly sports, define girl's primary interests. \
            Girl carries herself in a relaxed and confident way, acting elegant in any situation. Girl looks after herself and dresses with style. \
            By default, girl expects people to show the same level of communication and behavior as in her social circle. \
            Those who do not meet girl's inner standards evoke pity and rejection. Girl does not like ordinary people and boring stories.\
            """)
    return ""
