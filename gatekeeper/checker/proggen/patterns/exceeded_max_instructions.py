from __future__ import annotations
from .base import Pattern, PatternInfo
import random


class MonoTrustMiniRejectLong(Pattern):
    info = PatternInfo(
        name="mono_trust_mini_reject_long",
        description="Deterministic overlong loop to trigger max_instructions reject, "
        "with randomized nested loop structure and ministd ops.",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        outer = rng.randint(80, 300)
        mid = rng.randint(30, 80)
        inner = rng.randint(5, 20)
        variant = rng.randint(0, 1)
        return dict(
            LOOP_OUTER=outer,
            LOOP_MID=mid,
            LOOP_INNER=inner,
            PATTERN_VARIANT=variant,
        )


class ModuTrustMiniRejectLong(Pattern):
    info = PatternInfo(
        name="modu_trust_mini_reject_long",
        description="Modular version that always exceeds max_instructions via nested ministd + trusted loops.",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        outer = rng.randint(80, 200)
        mid = rng.randint(30, 70)
        inner = rng.randint(5, 15)
        variant = rng.randint(0, 1)
        return dict(
            LOOP_OUTER=outer,
            LOOP_MID=mid,
            LOOP_INNER=inner,
            PATTERN_VARIANT=variant,
        )


class MonoMiniRejectLong(Pattern):
    info = PatternInfo(
        name="mono_mini_reject_long",
        description="Monolithic ministd-only program that deterministically exceeds max_instructions via nested loops.",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        outer = rng.randint(100, 250)
        mid = rng.randint(40, 80)
        inner = rng.randint(8, 20)
        variant = rng.randint(0, 1)
        return dict(
            LOOP_OUTER=outer,
            LOOP_MID=mid,
            LOOP_INNER=inner,
            PATTERN_VARIANT=variant,
        )


class ModuMiniRejectLong(Pattern):
    info = PatternInfo(
        name="modu_mini_reject_long",
        description="Modular ministd-only program that deterministically exceeds max_instructions via nested loops.",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        file_size = rng.choice([64, 128, 256])
        outer = rng.randint(80, 200)
        mid = rng.randint(30, 80)
        inner = rng.randint(8, 20)
        variant = rng.randint(0, 1)
        return dict(
            FILE_SIZE=file_size,
            LOOP_OUTER=outer,
            LOOP_MID=mid,
            LOOP_INNER=inner,
            PATTERN_VARIANT=variant,
        )
