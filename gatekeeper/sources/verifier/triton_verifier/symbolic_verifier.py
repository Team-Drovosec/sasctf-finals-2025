"""
@Author: madrat
Skeleton implementation for a Triton-backed RISC-V symbolic verifier.
"""

from __future__ import annotations

import logging
import time
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Callable, List, NoReturn

from triton import (
    ARCH,
    CALLBACK,
    EXCEPTION,
    MODE,
    SOLVER,
    Instruction,
    MemoryAccess,
    TritonContext,
)

from .config import VerifierConfig
from .function_models import FunctionModels
from .instructions_whitelist import whitelist
from .memory_map import MemoryRegion, mm
from .memory_modeller import MemoryAccessType, MemoryModeller
from .solver_utils import bv, is_sat, pc_and

if TYPE_CHECKING:  # pragma: no cover
    from triton import Register

logger = logging.getLogger(__name__)


@dataclass(slots=True)
class VerificationReport:
    """Report produced by the symbolic verifier."""

    passed: bool = field(default=False)
    issue: str = field(default_factory=str)

    def __str__(self) -> str:
        if self.passed:
            return "Verification passed"
        return f"Verification failed: {self.issue}"


@dataclass(slots=True)
class VerificationException(Exception):
    """Exception with verification details."""

    passed: bool = field(default=False)
    issue: str = field(default_factory=str)

    def __str__(self) -> str:
        return self.issue


class TritonVerifier:
    """Triton-powered symbolic validator for user-supplied RISC-V binaries."""

    def __init__(self) -> None:
        self._cur_inst: Instruction | None = None

        arch = getattr(ARCH, "RV64", None)
        if arch is None:
            raise RuntimeError("Triton RISCV64 backend not available")

        self._ctx = TritonContext(arch)
        self._actx = self._ctx.getAstContext()

        self._ctx.setMode(MODE.ALIGNED_MEMORY, True)
        self._ctx.setMode(MODE.CONSTANT_FOLDING, True)
        self._ctx.setMode(MODE.AST_OPTIMIZATIONS, True)
        self._ctx.setMode(MODE.TAINT_THROUGH_POINTERS, True)

        self._ctx.setSolver(SOLVER.BITWUZLA)
        self._ctx.setSolverTimeout(VerifierConfig.solver_timeout_ms)
        self._ctx.setSolverMemoryLimit(VerifierConfig.solver_memory_limit)

        self._executable_regions = mm.get_executable_regions()

        self._trusted_funcs_region: MemoryRegion = mm.find_region_by_name("trusted-funcs")  # type: ignore[misc]
        assert self._trusted_funcs_region is not None

        self._allowed_trusted_funcs_call_targets: set[int] = {
            func.start for func in self._trusted_funcs_region.subregions
        }

        self._total_executed = 0

        self._function_models = FunctionModels(self)
        self._memory_modeller = MemoryModeller(self)
        self._pre_processing_hooks: List[Callable[[Instruction], None]] = [
            self._try_prove_jalr_has_only_1_target_if_symbolic,
        ]
        self._post_processing_hooks: List[Callable[[Instruction], None]] = [
            self._memory_modeller.model_symbolic_memory_read_hook,
        ]

        self._trusted_funcs_handlers = {
            "create_file_plain": self._function_models.model_create_file_plain,
            "create_file_enc": self._function_models.model_create_file_enc,
            "read_file_plain": self._function_models.model_read_file_plain,
            "read_file_enc": self._function_models.model_read_file_enc,
            "mini_printf": self._function_models.model_mini_printf,
        }

        logger.debug("Triton context initialized for RISC-V symbolic verification")

    def __repr__(self) -> str:
        return (
            f"<TritonVerifier max_program_size={VerifierConfig.full_program_size} "
            f"max_instructions={VerifierConfig.max_instructions}>"
        )

    def reject(self, issue: str, prefix: str = "") -> NoReturn:
        assert self._cur_inst is not None, "no current instruction set"
        issue = f"{prefix}instruction 0x{self._cur_inst.getAddress():016x}: {self._cur_inst.getDisassembly()} {issue}"
        raise VerificationException(passed=False, issue=issue)

    def _try_prove_jalr_has_only_1_target_if_symbolic(self, inst):
        if inst.getDisassembly().startswith("jalr"):
            target_reg = inst.getOperands()[0]
            reg_ast = self._ctx.getSymbolicRegister(target_reg).getAst()
            reg_val = self._ctx.getConcreteRegisterValue(target_reg)
            if (
                reg_ast.isSymbolized()
                and is_sat(
                    self._ctx,
                    pc_and(
                        self._ctx,
                        self._actx.bvult(
                            reg_ast, bv(self._actx, reg_val, reg_ast.getBitvectorSize())
                        ),
                    ),
                )
                or is_sat(
                    self._ctx,
                    pc_and(
                        self._ctx,
                        self._actx.bvugt(
                            reg_ast, bv(self._actx, reg_val, reg_ast.getBitvectorSize())
                        ),
                    ),
                )
            ):
                logger.debug(
                    "PC is symbolic, and can't be proven to only take 1 possible value, leaving as symbolic."
                )
            elif reg_ast.isSymbolized():
                logger.debug(
                    f"PC is symbolic, but has been proven to only take 1 possible value. Concretizing it to: 0x{reg_val:016x}"
                )
                self._ctx.setConcreteRegisterValue(target_reg, reg_val)
                self._ctx.setConcreteRegisterValue(self._ctx.registers.pc, reg_val)

    def _model_trusted_funcs_common(self) -> None:
        # Zero-out all caller-saved registers, to keep the state in-sync with
        # the executor's trusted-lib behavior (it scrubs caller-saved registers, so that
        # trusted functions can't leak information).
        SCRUB_REGS = [
            "x5",
            "x6",
            "x7",
            *[
                f"x{i}" for i in range(11, 18)
            ],  # a0 (x10) - is a returned value, so we don't scrub it
            *[f"x{i}" for i in range(28, 32)],
        ]

        for reg in self._ctx.getAllRegisters():
            reg_name = reg.getName()
            if reg_name in SCRUB_REGS:
                self._ctx.setConcreteRegisterValue(reg, 0)

    def _model_trusted_funcs(self, pc: int) -> None:
        if pc not in self._allowed_trusted_funcs_call_targets:
            self.reject(
                issue="is inside trusted-funcs, but is not allowed call target!",
            )

        name = mm.find_region(pc).name  # type: ignore[misc]
        routine_handler = self._trusted_funcs_handlers.get(name)
        assert (
            routine_handler is not None
        ), f"Trusted function handler for {name} not found!"

        routine_handler()
        self._model_trusted_funcs_common()

    def _verify_step_at(self, pc: int) -> None:
        inst_bytes = self._ctx.getConcreteMemoryAreaValue(pc, 4, False)
        self._cur_inst = Instruction(pc, inst_bytes)
        self._ctx.disassembly(self._cur_inst)
        # self._ctx.buildSemantics(self._cur_inst)

        disassembly = self._cur_inst.getDisassembly()

        # Check PC is not symbolic
        if self._ctx.isRegisterSymbolized(self._ctx.registers.pc):
            # @TODO: To actually model symbolic PC, we need some sort of a forking
            # mechanism here. For now, just reject.
            # But as a potential improvement, we could try to prove potential call
            # targets are valid? (e.g., they're only ending up in allowed call-targets from inside the trusted-funcs region?)
            # For now - just prove that symbolic PC could have only one possible value.
            self.reject("PC is symbolic!")

        # First do a coarse check: is the instruction in an executable region?
        if not any([region.contains(pc) for region in self._executable_regions]):
            self.reject("is outside executable regions!")

        # Do white-list checks on to-be executed instructions.
        if disassembly not in whitelist:
            self.reject("is not allowed!")

        # Now do a fine-grained check: if we're inside the trusted-funcs region,
        # ensure that we're only executing known wrappers
        if self._trusted_funcs_region.contains(pc):
            # Do extended checks on arguments/...
            self._model_trusted_funcs(pc)

        if disassembly.startswith("ebreak"):
            logger.info("Program accepted: hit ebreak instruction")
            raise VerificationException(passed=True)

        for hook in self._pre_processing_hooks:
            hook(self._cur_inst)

        match err := self._ctx.processing(self._cur_inst):
            case EXCEPTION.NO_FAULT:
                pass

            case _:
                self.reject(f"caused Triton exception {err}")

        # Post-processing hooks
        for hook in self._post_processing_hooks:
            hook(self._cur_inst)

        logger.debug(
            "(%05d) 0x%016x: %s",
            self._total_executed,
            pc,
            disassembly,
        )

    def verify_memory_range(
        self,
        start: int,
        length: int,
        access_type: MemoryAccessType = MemoryAccessType.READ,
    ) -> None:
        for addr in range(start, start + length):
            self._verify_memory_access(MemoryAccess(addr, 1), access_type)

    def _verify_memory_access(
        self,
        mem_access: MemoryAccess,
        access_type: MemoryAccessType,
    ) -> None:
        """This hook is executed on every memory access (read/write), and
        verifies that the access is allowed by the memory map, and some other
        sanity checks. It also notes all symbolic memory reads, which are
        processed in the post-processing hook.
        """

        settings = {
            MemoryAccessType.READ: (mm.is_read_allowed, "reads from"),
            MemoryAccessType.WRITE: (mm.is_write_allowed, "writes to"),
            MemoryAccessType.EXECUTE: (mm.is_execute_allowed, "executes from"),
        }
        predicate, msg = settings[access_type]

        addr = mem_access.getAddress()
        length = mem_access.getSize()

        assert length in [1, 2, 4, 8], "unexpected memory access size"

        # Basic check, only covers fully concrete accesses.
        # More complex checks (e.g., symbolic addresses) are done in the
        # post-processing hook.
        if not (predicate(addr) and predicate(addr + length - 1)):
            self.reject(f"{msg} disallowed address 0x{addr:016x}")

        # Disallow accesses to undefined (=tainted) memory for reads
        if self._ctx.isMemoryTainted(MemoryAccess(addr, length)):
            if access_type == MemoryAccessType.READ:
                self.reject(
                    f"attempted to access undefined memory at *0x{addr:016x}, size={length} = {self._ctx.getConcreteMemoryValue(MemoryAccess(addr, length), False):#0{length*2}x}",
                )

        if (lea_ast := mem_access.getLeaAst()) and lea_ast.isSymbolized():
            match access_type:
                # Fully disallow symbolic writes
                case MemoryAccessType.WRITE | MemoryAccessType.EXECUTE:
                    self.reject(
                        f"{msg} symbolic address {mem_access}",
                        prefix="[verify_memory_access] ",
                    )
                case MemoryAccessType.READ:
                    logger.debug(
                        f"[verify_memory_access] {msg} symbolic address {mem_access}"
                    )

                    # Actual memory modelling happens later, in the post-processing hook.
                    # @sa: _model_symbolic_memory_read_hook

                case _:
                    raise RuntimeError("unhandled memory access type")

    def _get_concrete_memory_value_hook(
        self,
        _: TritonContext,
        mem_access: MemoryAccess,
    ) -> None:
        self._verify_memory_access(mem_access, MemoryAccessType.READ)

    def _set_concrete_memory_value_hook(
        self,
        _: TritonContext,
        mem_access: MemoryAccess,
        __: int,
    ) -> None:
        self._verify_memory_access(mem_access, MemoryAccessType.WRITE)

    def _set_concrete_register_value_hook(
        self,
        _: TritonContext,
        reg: Register,
        new_sp: int,
    ) -> None:
        # We're only interested in SP
        if reg != self._ctx.registers.sp:
            return

        old_sp = self._ctx.getConcreteRegisterValue(reg)
        logger.debug("SP updated: 0x%016x -> 0x%016x", old_sp, new_sp)

        if new_sp < old_sp:
            # Stack growth - mark the new region as tainted, because real
            # functions might have pushed data onto the stack, that we don't
            # model.
            for addr in range(new_sp, old_sp):
                self._ctx.taintMemory(addr)
        elif new_sp > old_sp:
            # free: taint [old_sp, new_sp)
            for a in range(old_sp, new_sp):
                self._ctx.taintMemory(a)

    def _setup_program_context(self, program_blob: bytes):
        # Map RISC-V registers to ABI names
        self._ctx.registers.sp = self._ctx.registers.x2
        self._ctx.registers.ra = self._ctx.registers.x1

        # function args/return values
        self._ctx.registers.a0 = self._ctx.registers.x10
        self._ctx.registers.a1 = self._ctx.registers.x11

        # function args
        self._ctx.registers.a2 = self._ctx.registers.x12
        self._ctx.registers.a3 = self._ctx.registers.x13
        self._ctx.registers.a4 = self._ctx.registers.x14
        self._ctx.registers.a5 = self._ctx.registers.x15
        self._ctx.registers.a6 = self._ctx.registers.x16
        self._ctx.registers.a7 = self._ctx.registers.x17

        code_region: MemoryRegion = mm.find_region_by_name("user-code")  # type: ignore[misc]
        data_region: MemoryRegion = mm.find_region_by_name("user-data")  # type: ignore[misc]
        stack_region: MemoryRegion = mm.find_region_by_name("stack")  # type: ignore[misc]

        program_bytes = program_blob[: code_region.size()]
        data_bytes = program_blob[code_region.size() :]

        padded_code_blob = program_bytes.ljust(code_region.size(), b"\x00")
        self._ctx.setConcreteMemoryAreaValue(code_region.start, padded_code_blob)

        padded_data_blob = data_bytes.ljust(data_region.size(), b"\x00")
        self._ctx.setConcreteMemoryAreaValue(data_region.start, padded_data_blob)

        full_stack_blob = b"\x00" * stack_region.size()
        self._ctx.setConcreteMemoryAreaValue(stack_region.start, full_stack_blob)

        # Setup stack pointer
        self._ctx.setConcreteRegisterValue(
            self._ctx.registers.sp, stack_region.start + stack_region.size()
        )
        self._ctx.setConcreteRegisterValue(self._ctx.registers.pc, code_region.start)

        # Setup initial frame pointer
        self._ctx.setConcreteRegisterValue(
            self._ctx.registers.x8, stack_region.start + stack_region.size()
        )

        # model trusted functions as simple RETs (67800000)
        trusted_funcs_region = mm.find_region_by_name("trusted-funcs")
        assert trusted_funcs_region is not None

        for func in trusted_funcs_region.subregions:
            self._ctx.setConcreteMemoryAreaValue(func.start, bytes.fromhex("67800000"))

        # Memory Hooks
        self._ctx.addCallback(
            CALLBACK.GET_CONCRETE_MEMORY_VALUE,
            self._get_concrete_memory_value_hook,
        )

        self._ctx.addCallback(
            CALLBACK.SET_CONCRETE_MEMORY_VALUE,
            self._set_concrete_memory_value_hook,
        )

        # Register Hooks
        self._ctx.addCallback(
            CALLBACK.SET_CONCRETE_REGISTER_VALUE,
            self._set_concrete_register_value_hook,
        )

        stack_region = mm.find_region_by_name("stack")  # type: ignore[misc]
        assert stack_region is not None, "stack region not found in memory map!"
        for addr in range(stack_region.start, stack_region.end):
            self._ctx.taintMemory(addr)

    def _do_verify(self, program_blob: bytes) -> None:
        self._setup_program_context(program_blob)

        start_time = time.time()
        while self._total_executed < VerifierConfig.max_instructions:
            pc = self._ctx.getConcreteRegisterValue(self._ctx.registers.pc)
            self._verify_step_at(pc)
            self._total_executed += 1
            if time.time() - start_time > VerifierConfig.total_verification_time_s:
                self.reject(
                    f"verification timed out after {VerifierConfig.total_verification_time_s} seconds"
                )

        self.reject(f"exceeded max instructions {VerifierConfig.max_instructions}")

    def verify_program(self, program_blob: bytes) -> VerificationReport:
        if len(program_blob) > VerifierConfig.full_program_size:
            return VerificationReport(
                passed=False,
                issue=(
                    f"program size {len(program_blob)} exceeds limit "
                    f"{VerifierConfig.full_program_size}"
                ),
            )

        start_time = time.time()
        try:
            self._do_verify(program_blob)
        except VerificationException as e:
            passed = getattr(e, "passed", False)
            issue = getattr(e, "issue", None)
            if passed:
                return VerificationReport(passed=True)
            return VerificationReport(
                passed=False, issue=issue or "verification failed"
            )
        except Exception as e:
            return VerificationReport(passed=False, issue=f"internal error: {e!s}")
        finally:
            elapsed = time.time() - start_time
            logger.info(
                "Verification finished in %.2f seconds, executed %d instructions",
                elapsed,
                self._total_executed,
            )

        # If _do_verify ever returns normally (it likely never does), treat as unknown.
        return VerificationReport(passed=False, issue="unknown error")
