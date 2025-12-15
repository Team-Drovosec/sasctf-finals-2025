from __future__ import annotations
import random
from .base import Pattern, PatternInfo


class InternalErrorFailedToDisassemble(Pattern):
    info = PatternInfo(
        name="internal_error_failed_to_disassemble",
        description="Program failed to disassemble given instruction.",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        return dict()
