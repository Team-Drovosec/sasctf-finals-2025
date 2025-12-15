"""
@Author: madrat
Configuration utilities for the Triton-based verifier.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import ClassVar

from .memory_map import mm

logger = logging.getLogger(__name__)


@dataclass(slots=True)
class VerifierConfig:
    """Configuration for symbolic verification."""

    max_code_size: ClassVar[int] = mm.find_region_by_name("user-code").size()
    max_data_size: ClassVar[int] = mm.find_region_by_name("user-data").size()
    full_program_size: ClassVar[int] = max_code_size + max_data_size

    max_file_size: ClassVar[int] = 2 * 1024  # 2 KB

    max_instructions: ClassVar[int] = 25000

    total_verification_time_s: ClassVar[int] = 25  # seconds
    solver_timeout_ms: ClassVar[int] = 5000  # milliseconds
    solver_memory_limit: ClassVar[int] = 1024 * 1024 * 1024  # 512 MB
    max_symbolic_read_elems: ClassVar[int] = 0x100
