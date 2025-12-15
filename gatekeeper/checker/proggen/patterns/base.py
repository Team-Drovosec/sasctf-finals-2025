from __future__ import annotations
from dataclasses import dataclass
from typing import Dict, Any
import abc
import random


@dataclass(frozen=True)
class PatternInfo:
    name: str
    description: str = ""
    category: str = "reject"  # either "reject", "get-enc", "get-plain", or "accept"
    checker_message: str | None = (
        None
    )


class Pattern(abc.ABC):
    """Base for all patterns."""

    info: PatternInfo
    template_suffix: str = ".c.j2"
    template_override: str | None = None

    @abc.abstractmethod
    def sample_params(self, rng: random.Random) -> Dict[str, Any]:
        raise NotImplementedError

    def template_name(self) -> str:
        if self.template_override:
            return self.template_override

        module_name = self.__class__.__module__.split(".")[-1]
        class_name = self.__class__.__name__
        return f"{module_name}/{class_name}{self.template_suffix}"
