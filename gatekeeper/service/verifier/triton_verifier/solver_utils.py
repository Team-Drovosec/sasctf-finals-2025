"""
@Author: madrat
Solver utility functions for Triton-based symbolic verification.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Dict, Tuple

from triton import SOLVER_STATE, TritonContext

if TYPE_CHECKING:  # pragma: no cover
    from triton import AstNode


def bv(actx, v: int, bits: int):
    return actx.bv(int(v) & ((1 << bits) - 1), bits)


def get_model(ctx: TritonContext, node: "AstNode") -> Tuple[Dict, SOLVER_STATE]:
    # Fail-closed on unknown/timeout (Triton returns None)
    model, status, solving_time_ms = ctx.getModel(node, True)

    str_status = {
        SOLVER_STATE.SAT: "SAT",
        SOLVER_STATE.UNSAT: "UNSAT",
        SOLVER_STATE.UNKNOWN: "UNKNOWN",
        SOLVER_STATE.TIMEOUT: "TIMEOUT",
        SOLVER_STATE.OUTOFMEM: "OUTOFMEM",
    }

    # logger.debug(
    #     "SMT query status: %s (solving time: %.2f ms)",
    #     str_status.get(status, "UNKNOWN"),
    #     solving_time_ms,
    # )

    if status in [
        SOLVER_STATE.UNKNOWN,
        SOLVER_STATE.TIMEOUT,
        SOLVER_STATE.OUTOFMEM,
    ]:
        raise ValueError(f"SMT solver failed to solve the query (status: {status})")
    return (model, status)


def is_sat(ctx: TritonContext, node: "AstNode") -> bool:
    model, status = get_model(ctx, node)
    return status == SOLVER_STATE.SAT and model is not None


def is_unsat(ctx: TritonContext, node: "AstNode") -> bool:
    _, status = get_model(ctx, node)
    return status == SOLVER_STATE.UNSAT


def pc_and(ctx: TritonContext, *nodes: "AstNode") -> "AstNode":
    conj = [ctx.getPathPredicate()]
    conj.extend(nodes)
    return ctx.getAstContext().land(conj)
