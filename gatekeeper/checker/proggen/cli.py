from __future__ import annotations
import argparse
from pathlib import Path
from .engine import ProgramGenerator


def main():
    ap = argparse.ArgumentParser(prog="proggen")
    sub = ap.add_subparsers(dest="cmd", required=True)

    # common flags for both subcommands
    common = argparse.ArgumentParser(add_help=False)
    common.add_argument("--make-dir", type=Path, default=Path("."), required=True)

    sub.add_parser("list", parents=[common], help="list targets")
    gen = sub.add_parser(
        "generate", parents=[common], help="generate and build one program"
    )
    gen.add_argument("--target", required=True)
    gen.add_argument("--seed", type=int, required=True)
    gen.add_argument("--out", type=Path, default=Path("out"))

    args = ap.parse_args()
    root = Path(__file__).resolve().parents[1]
    pg = ProgramGenerator(root=root, make_dir=args.make_dir)

    if args.cmd == "list":
        targets = pg.list_targets()

        if targets["modules"]:
            for module, children in targets["modules"].items():
                print(f"{module} (random pool):")
                for child in children:
                    print(f"  - {child}")

        for name in targets["patterns"]:
            print(name)
    else:
        res = pg.generate(args.target, args.seed, args.out)
        print(f"OK: {res.bin_path}")
        print(f"   src: {res.src_path}")
        print(f"   meta:{res.meta_path}")
