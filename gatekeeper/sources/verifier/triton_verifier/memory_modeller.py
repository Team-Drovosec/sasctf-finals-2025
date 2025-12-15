"""
@Author: madrat
Triton memory modeller for symbolic memory reads.
"""

from __future__ import annotations

import enum
import logging
from typing import TYPE_CHECKING, List

from triton import OPERAND, Instruction, MemoryAccess

from .config import VerifierConfig
from .solver_utils import bv, is_sat, pc_and

logger = logging.getLogger(__name__)

if TYPE_CHECKING:  # pragma: no cover
    from triton import AstNode, Register

    from .symbolic_verifier import TritonVerifier


class MemoryAccessType(enum.Enum):
    READ = enum.auto()
    WRITE = enum.auto()
    EXECUTE = enum.auto()


class MemoryModeller:
    """A class that adds support for modeling symbolic memory reads in Triton.

    Mostly based on building ITE expressions for symbolic addresses, as described in: (Towards Symbolic Pointers Reasoning in Dynamic Symbolic Execution) https://arxiv.org/pdf/2109.03698
    """

    def __init__(self, verifier: "TritonVerifier") -> None:
        self.verifier = verifier
        self._ctx = verifier._ctx
        self._actx = verifier._ctx.getAstContext()

    def _binary_search_bound(
        self,
        node: "AstNode",
        low: int,
        high: int,
        find_upper: bool,
    ) -> int:
        assert low <= high, "invalid binary search bounds"

        while low < high:
            mid = (low + high) // 2
            check = (
                pc_and(
                    self._ctx,
                    self._actx.bvuge(
                        node, bv(self._actx, mid, node.getBitvectorSize())
                    ),
                )
                if find_upper
                else pc_and(
                    self._ctx,
                    self._actx.bvule(
                        node, bv(self._actx, mid, node.getBitvectorSize())
                    ),
                )
            )

            if is_sat(self._ctx, check):
                if find_upper:
                    low = mid + 1
                else:
                    high = mid
            else:
                if find_upper:
                    high = mid
                else:
                    low = mid + 1

        return low if find_upper else high

    def _prove_mem_access_aligned(self, lea_ast: "AstNode", size: int) -> bool:
        """Prove that the memory access is aligned to `size` bytes.
        I.e., lea_ast % size == 0.
        """
        assert size in [1, 2, 4, 8], "unsupported memory access size"

        ast = self._actx
        bits = lea_ast.getBitvectorSize()
        mask = bv(self._actx, size - 1, bits)

        cond = pc_and(
            self._ctx,
            ast.distinct(ast.bvand(lea_ast, mask), bv(self._actx, 0, bits)),
        )

        return not is_sat(self._ctx, cond)

    def _build_ite_for_memory_access(
        self, lea: "AstNode", size: int, addresses: List[int]
    ) -> "AstNode":
        assert addresses, "no addresses to build ITE for"
        ast = self._ctx.getAstContext()
        ea_bw = lea.getBitvectorSize()
        val_bw = size * 8

        # @TODO: Optimize by building linear formulas for segments with concrete-only values. Also use balanced ITE trees (build a BST).

        expr = bv(self._actx, 0, val_bw)
        for addr in reversed(addresses):
            cond = ast.equal(lea, bv(self._actx, addr, ea_bw))
            val = self._ctx.getMemoryAst(MemoryAccess(addr, size))
            assert val.getBitvectorSize() == val_bw
            expr = ast.ite(cond, val, expr)

        return expr

    def _get_load_dst_reg(self, inst: Instruction) -> "Register | None":
        # Works for lb/lbu/lh/lhu/lw/lwu/ld
        ops = inst.getOperands()
        if (
            len(ops) >= 2
            and ops[0].getType() == OPERAND.REG
            and ops[1].getType() == OPERAND.MEM
        ):
            return ops[0]
        return None

    def _assign_load_result(self, inst: Instruction, value_ast: "AstNode") -> None:
        dst = self._get_load_dst_reg(inst)
        if dst is None:
            self.verifier.reject(
                "failed to locate destination register for symbolic load"
            )

        # Apply RV64 load extension based on opcode
        op = inst.getDisassembly().split()[0]  # lb/lbu/lh/lhu/lw/lwu/ld
        ast = self._actx
        dst_bits = dst.getBitSize()
        w = value_ast.getBitvectorSize()

        def zx(to_bits):
            return ast.zx(to_bits - w, value_ast) if to_bits > w else value_ast

        def sx(to_bits):
            return ast.sx(to_bits - w, value_ast) if to_bits > w else value_ast

        class LoadOp:
            LB = "lb"
            LH = "lh"
            LW = "lw"
            LBU = "lbu"
            LHU = "lhu"
            LWU = "lwu"
            LD = "ld"

        match op:
            case LoadOp.LB | LoadOp.LH:
                value_ast = sx(dst_bits)
            case LoadOp.LBU | LoadOp.LHU:
                value_ast = zx(dst_bits)
            case LoadOp.LW:
                if w != 32:
                    value_ast = ast.extract(31, 0, value_ast)
                value_ast = (
                    ast.sx(dst_bits - 32, value_ast) if dst_bits > 32 else value_ast
                )
            case LoadOp.LWU:
                if w != 32:
                    value_ast = ast.extract(31, 0, value_ast)
                value_ast = (
                    ast.zx(dst_bits - 32, value_ast) if dst_bits > 32 else value_ast
                )
            case LoadOp.LD:
                if w < dst_bits:
                    value_ast = ast.zx(dst_bits - w, value_ast)
            case _:
                self.verifier.reject("unsupported load opcode for symbolic load")

        sexpr = self._ctx.newSymbolicExpression(
            value_ast, f"modeled_symbolic_read_ite_{self.verifier._total_executed}"
        )
        self._ctx.assignSymbolicExpressionToRegister(sexpr, dst)

    def model_symbolic_memory_read_hook(self, inst: Instruction) -> None:
        for ma, _ in inst.getLoadAccess():
            ma: MemoryAccess

            lea = ma.getLeaAst()
            if not (lea and lea.isSymbolized()):
                continue

            size = ma.getSize()
            bits = lea.getBitvectorSize()
            anchor = ma.getAddress()  # Triton's concrete EA for this execution

            # 1. Assert that the symbolic memory access is bounded by some constant - e.g., 0x100.
            #    So we basically check that:
            #       solve(mem_access >= mem_access + 0x101 * mem_access.getSize()) == UNSAT (upper bound)
            #       solve(mem_access <= mem_access - 0x101 * mem_access.getSize()) == UNSAT (lower bound)
            #
            # @TODO: It's not a fully complete approach, as it might miss some cases
            # where potential addresses are not contiguous (basically allowed regions
            # are disjoint). So we might reject some valid cases, however we're safe in
            # the sense that we won't miss any unsafe cases.
            max_elems = VerifierConfig.max_symbolic_read_elems
            upper_start_limit = anchor + (max_elems) * size
            lower_start_limit = (
                anchor - max_elems * size if anchor >= max_elems * size else 0
            )

            upper_check = pc_and(
                self._ctx,
                self._actx.bvugt(lea, bv(self._actx, upper_start_limit, bits)),
            )
            lower_check = pc_and(
                self._ctx,
                self._actx.bvult(lea, bv(self._actx, lower_start_limit, bits)),
            )

            # If ∃ model beyond limits, reject (fail-closed).
            if is_sat(self._ctx, upper_check):
                self.verifier.reject(
                    f"reads from symbolic address {ma} unbounded above (>{max_elems} elements)!",
                )
            if is_sat(self._ctx, lower_check):
                self.verifier.reject(
                    f"reads from symbolic address {ma} unbounded below (>{max_elems} elements)!",
                )

            # 2. If both checks pass, we can extract the bounds by binary searching
            #    for the lowest/highest address that still satisfies the above constraints.
            upper_start_exact = self._binary_search_bound(
                lea, anchor, upper_start_limit, True
            )
            lower_start_exact = self._binary_search_bound(
                lea, lower_start_limit, anchor, False
            )

            # 3. Prove address alignment to stride (power-of-two sizes on RV64).
            #    This helps us limit the number of ITE cases we need to consider.
            #    For example, if we read 8 bytes, and we know the address is aligned
            #    to 8 bytes, we only need to consider 1 case per 8-byte chunk in the range.
            #    Without this, we would need to consider 8 cases per 8-byte chunk (1 byte steps).
            if not self._prove_mem_access_aligned(lea, size):
                self.verifier.reject(
                    f"reads from symbolic address {ma} with non-aligned access (size={size})",
                )

            def align_up(v, a):
                return ((v + (a - 1)) // a) * a

            def align_down(v, a):
                return (v // a) * a

            lower_start_exact = align_up(lower_start_exact, size)
            upper_start_exact = align_down(upper_start_exact, size)

            if upper_start_exact < lower_start_exact:
                self.verifier.reject(
                    f"symbolic address window collapsed after alignment for {ma}"
                )

            logger.debug(
                "instruction 0x%016x: %s reads from symbolic address %s bounds=[0x%016x, 0x%016x]",
                inst.getAddress(),
                inst.getDisassembly(),
                ma,
                lower_start_exact,
                upper_start_exact,
            )

            # 4. Filter-out only feasible addresses in the [lower, upper] range
            #    that satisfy the path constraints. We already know that the
            #    addresses are aligned to `size`, so we can step by `size`.
            candidates = list(range(lower_start_exact, upper_start_exact + 1, size))
            feasible = [
                a
                for a in candidates
                if is_sat(
                    self._ctx,
                    pc_and(self._ctx, self._actx.equal(lea, bv(self._actx, a, bits))),
                )
            ]

            if not feasible:
                self.verifier.reject(
                    f"no feasible concrete addresses for symbolic read {ma} after constraints"
                )

            if logger.isEnabledFor(logging.DEBUG):
                logger.debug(f"Potential addresses for {ma}:")
                for addr in feasible:
                    val = self._ctx.getConcreteMemoryValue(
                        MemoryAccess(addr, size), False
                    )
                    is_symbolic = self._ctx.isMemorySymbolized(MemoryAccess(addr, size))
                    is_tainted = self._ctx.isMemoryTainted(MemoryAccess(addr, size))
                    logger.debug(
                        f"  0x{addr:016x} -> 0x{val:0{size*2}x} {'(sym)' if is_symbolic else '(concrete)'} {'(tainted)' if is_tainted else ''}",
                    )

            # 5. Check that none of the feasible addresses are tainted or unreadable.
            for addr in feasible:
                if self._ctx.isMemoryTainted(MemoryAccess(addr, size)):
                    self.verifier.reject(
                        f"might access tainted memory at 0x{addr:08x} through symbolic pointer {ma}",
                    )
                self.verifier.verify_memory_range(addr, size, MemoryAccessType.READ)

            # 6. Finally, we can build an ITE formula for all the addresses in the
            #    range [lower, upper], and use that as the destination expression.
            expr = self._build_ite_for_memory_access(lea, size, feasible)
            self._assign_load_result(inst, expr)
