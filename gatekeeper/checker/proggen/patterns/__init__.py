from __future__ import annotations

from collections import defaultdict
import importlib
import inspect
import pkgutil
from pathlib import Path
from typing import Dict, Tuple, Type
import warnings

from .base import Pattern


def _discover_patterns() -> Tuple[Type[Pattern], ...]:
    pkg_name = __name__
    pkg_path = Path(__file__).resolve().parent

    patterns: list[Type[Pattern]] = []

    for module_info in pkgutil.iter_modules([str(pkg_path)]):
        if module_info.ispkg:
            continue

        mod_name = module_info.name
        if mod_name in {"__init__", "base"} or mod_name.startswith("_"):
            continue

        module = importlib.import_module(f"{pkg_name}.{mod_name}")

        for obj in vars(module).values():
            if (
                not inspect.isclass(obj)
                or not issubclass(obj, Pattern)
                or obj is Pattern
            ):
                continue

            if getattr(obj, "info", None) is None:
                warnings.warn(
                    f"Pattern '{obj.__qualname__}' from module '{module.__name__}' lacks 'info'; skipping.",
                    RuntimeWarning,
                    stacklevel=2,
                )
                continue

            patterns.append(obj)

    return tuple(patterns)


_PATTERNS: Tuple[Type[Pattern], ...] = _discover_patterns()
_PATTERN_REGISTRY: Dict[str, Type[Pattern]] = {}
_MODULE_REGISTRY: dict[str, list[Type[Pattern]]] = defaultdict(list)

for cls in _PATTERNS:
    module_key = cls.__module__.split(".")[-1]
    _MODULE_REGISTRY[module_key].append(cls)

    key = cls.info.name
    if key in _PATTERN_REGISTRY:
        warnings.warn(
            (
                f"Duplicate pattern name '{key}' detected in '{cls.__module__}.{cls.__name__}'. "
                "Keeping the first definition."
            ),
            RuntimeWarning,
            stacklevel=2,
        )
        continue

    _PATTERN_REGISTRY[key] = cls


def registry() -> Dict[str, Type[Pattern]]:
    return dict(_PATTERN_REGISTRY)


def module_registry() -> Dict[str, Tuple[Type[Pattern], ...]]:
    return {
        module: tuple(sorted(classes, key=lambda cls: cls.info.name))
        for module, classes in _MODULE_REGISTRY.items()
    }


__all__ = ["registry", "module_registry"]
