from __future__ import annotations
from pathlib import Path
import subprocess


class CompileError(RuntimeError):
    pass


def make_compile(
    makefile_dir: Path,
    src: Path,
    out_dir: Path,
    extra_cflags: str = "",
    target="prog",
    comptime_seed: int = 1,
) -> Path:
    out_dir.mkdir(parents=True, exist_ok=True)
    log = (out_dir / "build.log").open("w")

    cmd = [
        "make",
        "-C",
        str(makefile_dir),
        f"SRCS={src.absolute()}",
        f"BUILD_DIR={out_dir.absolute()}",
        f"EXE_NAME={target}",
        f"COMPTIME_SEED={comptime_seed}",
        f"EXTRA_CFLAGS={extra_cflags}",
        "all",
    ]
    proc = subprocess.run(cmd, stdout=log, stderr=subprocess.STDOUT)
    log.close()
    if proc.returncode != 0:
        raise CompileError(f"make failed; see {out_dir/'build.log'}")
    return out_dir / f"{target}.bin"
