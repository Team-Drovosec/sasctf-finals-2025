from __future__ import annotations

import json
import random
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict

from .compile import make_compile
from .jinja_env import build_env
from .patterns import module_registry, registry
from .patterns.base import PatternInfo


@dataclass
class GenResult:
    template: str
    seed: int
    out_dir: Path
    src_path: Path
    bin_path: Path
    meta_path: Path


def _deep_merge(dst: Dict[str, Any], src: Dict[str, Any]) -> Dict[str, Any]:
    out = dict(dst)
    for k, v in src.items():
        if isinstance(v, dict) and isinstance(out.get(k), dict):
            out[k] = _deep_merge(out[k], v)
        else:
            out[k] = v
    return out


class ProgramGenerator:
    def __init__(self, root: Path, make_dir: Path):
        self.root = root
        self.make_dir = make_dir
        self.templates = build_env(root / "proggen" / "patterns" / "templates")
        self._registry = registry()
        self._modules = module_registry()

    def list_targets(self) -> Dict[str, Any]:
        modules = {
            module: sorted((cls.info.name) for cls in classes)
            for module, classes in sorted(self._modules.items())
        }
        patterns = sorted(self._registry.keys())
        return {"modules": modules, "patterns": patterns}

    def list_modules_filter_by_category(self, category: str):
        # Given a category of 'target (either 'reject' or 'accept'), return filtered list of targets.
        return {
            module: sorted(
                cls.info.name for cls in classes if cls.info.category == category
            )
            for module, classes in sorted(self._modules.items())
        }

    def get_target_info(self, target: str) -> PatternInfo:
        Pat = self._registry[target]
        return Pat.info

    def sample_random_cflags(self, rng: random.Random) -> str:
        # Randomize various compile-flags using extra_cflags.
        possible_opts = [
            "-O0",
            "-O1",
            "-O2",
            "-O3",
            "-Os",
            "-Ofast",
        ]
        possible_flags = [
            ("-fno-unroll-loops", "-funroll-loops"),
            ("-fno-inline-functions", "-finline-functions"),
            ("-fno-strict-aliasing", "-fstrict-aliasing"),
        ]

        opt_level = rng.choice(possible_opts)
        flags = rng.sample(possible_flags, k=rng.randint(0, len(possible_flags)))
        return " ".join([opt_level] + [rng.choice(pair) for pair in flags])

    def generate(
        self,
        pattern: str,
        seed: int,
        out_base: Path,
        overrides: Dict[str, Any] | None = None,
    ) -> GenResult:
        rng = random.Random(seed)

        if pattern in self._registry:
            Pat = self._registry[pattern]
        else:
            raise ValueError(f"Unknown pattern: {pattern}")

        pat = Pat()

        extra_cflags = self.sample_random_cflags(rng)
        params = pat.sample_params(rng)
        params.update(extra_cflags=extra_cflags)

        if overrides:
            params = _deep_merge(params, overrides)

        params.update(pattern_name=pattern, seed=seed)

        template = self.templates.get_template(pat.template_name())
        src_code = template.render(**params)

        out_dir = out_base / pattern / str(seed)
        out_dir.mkdir(parents=True, exist_ok=True)
        src_path = out_dir / "main.c"
        src_path.write_text(src_code)

        meta_path = out_dir / "meta.json"
        # meta_path.write_text(json.dumps(params, indent=2))

        bin_path = make_compile(
            self.make_dir,
            src_path,
            out_dir,
            extra_cflags=extra_cflags,
            target="prog",
            comptime_seed=seed,
        )

        return GenResult(pattern, seed, out_dir, src_path, bin_path, meta_path)

    def generate_binary(
        self,
        pattern: str,
        seed: int,
        out_base: Path,
        overrides: Dict[str, Any] | None = None,
    ) -> bytes:
        res = self.generate(pattern, seed, out_base, overrides)
        with res.bin_path.open("rb") as f:
            return f.read()
