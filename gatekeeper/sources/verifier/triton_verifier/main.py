#!/usr/bin/python3
# -*- coding: utf-8 -*-
# @Author: madrat

from __future__ import annotations

import argparse
import logging
import pathlib

from .symbolic_verifier import TritonVerifier

logging.basicConfig(
    level=logging.DEBUG, format="[%(asctime)s] %(message)s", datefmt="%H:%M:%S"
)


def main():
    parser = argparse.ArgumentParser(description="Triton Verifier")
    parser.add_argument(
        "program", help="Path to the program to verify", type=pathlib.Path
    )
    args = parser.parse_args()

    verifier = TritonVerifier()
    report = verifier.verify_program(args.program.read_bytes())
    print(report)


if __name__ == "__main__":
    main()
