from __future__ import annotations
from jinja2 import Environment, FileSystemLoader, StrictUndefined
from pathlib import Path


def _ull(x: int) -> str:
    return f"0x{x:016x}ULL"


def build_env(templates_dir: Path) -> Environment:
    env = Environment(
        loader=FileSystemLoader(str(templates_dir)),
        autoescape=False,
        undefined=StrictUndefined,
        trim_blocks=True,
        lstrip_blocks=True,
    )
    env.filters["ull"] = _ull
    return env
