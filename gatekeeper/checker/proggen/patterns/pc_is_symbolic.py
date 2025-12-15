from __future__ import annotations
from .base import Pattern, PatternInfo
import random


class MonoTrustMiniRejectSymbolic(Pattern):
    info = PatternInfo(
        name="mono_trust_mini_reject_symbolic",
        description="Rich trusted/ministd hybrid that triggers symbolic PC via branches on trusted results.",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        return dict(
            FILE_SIZE=rng.choice([64, 128, 256]),
            COUNT_PLAIN=rng.randint(2, 4),
            COUNT_ENC=rng.randint(1, 3),
            FORMAT_CASE=rng.randint(0, 1),
        )


class ModuTrustMiniRejectSymbolic(Pattern):
    info = PatternInfo(
        name="modu_trust_mini_reject_symbolic",
        description="Modular trusted/ministd hybrid that triggers symbolic PC via branch on trusted results.",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        return dict(
            FILE_SIZE=rng.choice([64, 128, 256]),
            COUNT_PLAIN=rng.randint(2, 4),
            FORMAT_CASE=rng.randint(0, 1),
        )


class MonoMiniRejectSymbolic(Pattern):
    info = PatternInfo(
        name="mono_mini_reject_symbolic",
        description="Monolithic ministd+trusted hybrid: branches depend on trusted result → PC symbolic.",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        return dict(
            FILE_SIZE=rng.choice([64, 128, 256]),
            seed=rng.randint(0, 9999),
        )


class ModuMiniRejectSymbolic(Pattern):
    info = PatternInfo(
        name="modu_mini_reject_symbolic",
        description="Modular ministd+trusted hybrid with symbolic branches → PC is symbolic.",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        return dict(
            FILE_SIZE=rng.choice([64, 128, 256]),
            seed=rng.randint(0, 9999),
        )


class MonoTrustRejectSymbolic(Pattern):
    info = PatternInfo(
        name="mono_trust_reject_symbolic",
        description="Trusted-only monolithic pattern with branch on trusted result → PC is symbolic.",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        return dict(
            FILE_SIZE=rng.choice([64, 128, 256]),
            COUNT_PLAIN=rng.randint(2, 4),
            COUNT_ENC=rng.randint(1, 3),
            FORMAT_CASE=rng.randint(0, 1),
            seed=rng.randint(0, 9999),
        )
