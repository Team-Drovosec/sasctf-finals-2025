#!/usr/bin/env python3
from pathlib import Path
import sys

PROJECT_PATH = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(PROJECT_PATH))

from proggen.engine import ProgramGenerator
from sources.verifier.triton_verifier.symbolic_verifier import TritonVerifier


# import logging
# logging.basicConfig(
#     level=logging.DEBUG, format="[%(asctime)s] %(message)s", datefmt="%H:%M:%S"
# )
# logger = logging.getLogger(__name__)


def main():
    for seed in range(50):
        target = "modu_trust_tainted"

        make_dir = PROJECT_PATH / "service" / "example_program"
        pg = ProgramGenerator(
            root=       PROJECT_PATH / "checker",
            make_dir=   make_dir
        )
        res = pg.generate(
            pattern=    target,
            seed=       seed,
            out_base=   make_dir
        )

        tv = TritonVerifier()
        report = tv.verify_program(res.bin_path.read_bytes())
        print(f"{seed}: {report}")


if __name__ == "__main__":
    main()