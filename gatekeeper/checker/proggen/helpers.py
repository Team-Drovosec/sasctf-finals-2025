"""Utility helpers for generating randomized C snippets."""

from __future__ import annotations

import random
from textwrap import indent
from typing import Iterable, List

ASCII_ALPHABET = "abcdefghijklmnopqrstuvwxyz"
ASCII_ALPHANUM = ASCII_ALPHABET + ASCII_ALPHABET.upper() + "0123456789"


def random_identifier(rnd: random.Random, prefix: str = "sym") -> str:
    suffix = "".join(rnd.choice(ASCII_ALPHANUM) for _ in range(6))
    return f"{prefix}_{suffix}"


def random_hex_literal(rnd: random.Random, bits: int = 64) -> str:
    width = bits // 4
    value = rnd.getrandbits(bits)
    return f"0x{value:0{width}x}"


def random_ascii_string(rnd: random.Random, min_len: int = 1, max_len: int = 24) -> str:
    length = rnd.randint(min_len, max_len)
    chars = [rnd.choice(ASCII_ALPHABET + " ") for _ in range(length)]
    return "".join(chars).strip() or "test"


def format_c_initializer(values: Iterable[int], per_line: int = 8) -> str:
    rows: List[str] = []
    current: List[str] = []
    for value in values:
        current.append(f"0x{value:08x}")
        if len(current) >= per_line:
            rows.append(", ".join(current))
            current = []
    if current:
        rows.append(", ".join(current))
    if not rows:
        return ""
    if len(rows) == 1:
        return rows[0]
    return ",\n".join(
        indent(row, "    ") if idx else row for idx, row in enumerate(rows)
    )


def escape_c_string(text: str) -> str:
    return text.replace("\\", "\\\\").replace('"', '\\"')
