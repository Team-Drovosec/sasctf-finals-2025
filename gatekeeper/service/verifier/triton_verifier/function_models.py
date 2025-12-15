"""
@Author: madrat
Function models for Triton symbolic verifier.
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, NoReturn, Tuple

from triton import MemoryAccess

from .config import VerifierConfig
from .memory_modeller import MemoryAccessType

if TYPE_CHECKING:  # pragma: no cover
    from .symbolic_verifier import TritonVerifier


logger = logging.getLogger(__name__)


class FunctionModels:
    def __init__(self, verifier: TritonVerifier) -> None:
        self._verifier = verifier
        self._ctx = verifier._ctx
        self._total_executed = 0

    def reject(self, reason: str) -> NoReturn:
        self._verifier.reject(reason)

    def _get_concrete_memory_string_checked(
        self, addr: int, max_len: int = 4096
    ) -> bytes:
        """Helper to read a null-terminated string from concrete memory,
        verifying that all bytes are in readable memory and not tainted."""

        s = bytearray()
        # We specifically run with `getConcreteMemoryValue(addr, True)` to
        # ensure all memory checks are performed.
        while c := self._ctx.getConcreteMemoryValue(addr, True):
            s.append(c)
            addr += 1

            if len(s) >= max_len:
                self.reject("string too long or unterminated!")

        return bytes(s)

    def _extract_concrete_args(
        self, num_args: int, mask: int = 0xFFFFFFFFFFFFFFFF
    ) -> Tuple[int, ...]:
        args = []

        for arg_idx in range(num_args):
            register_name = f"a{arg_idx}"
            cur_register = getattr(self._ctx.registers, register_name, None)
            assert cur_register is not None, f"register {register_name} not found!"

            arg_value = self._ctx.getConcreteRegisterValue(cur_register)
            args.append(arg_value & mask)

        return tuple(args)

    def model_create_file_plain(self) -> None:
        # uint64_t create_file_plain([[in]] uint8_t* fbytes, [[in]] uint32_t len);

        # 1. Check `fbytes` pointer is not symbolic, also `len` is not symbolic too.
        # 2. Check [`fbytes`, `fbytes + len`] \in readable memory, and the memory itself
        #    is not tainted (i.e., we're not reading from uninitialized memory). But it
        #    can be symbolic (e.g., encrypted data).
        # 3. Model the return value as a new symbolic variable.

        # a0=fbytes, a1=len
        # pylint: disable=unbalanced-tuple-unpacking
        fbytes, fsize = self._extract_concrete_args(2)

        logger.debug("create_file_plain(fbytes=0x%016x, len=%u)", fbytes, fsize)

        # 1) Pointers/len must be non-symbolic
        if self._ctx.isRegisterSymbolized(self._ctx.registers.a0):
            self.reject("create_file_plain: `fbytes` pointer is symbolic!")
        if self._ctx.isRegisterSymbolized(self._ctx.registers.a1):
            self.reject("create_file_plain: `len` is symbolic!")

        if fsize > VerifierConfig.max_file_size:
            self.reject(
                f"create_file_plain: fsize {fsize} exceeds maximum allowed size!"
            )
        elif fsize % 4 != 0:
            self.reject("create_file_plain: fsize must be multiple of 4!")
        elif fsize == 0:
            self.reject("create_file_plain: fsize cannot be zero!")

        # Verify fbytes is in readable memory and not tainted.
        self._verifier.verify_memory_range(fbytes, fsize)

        # 3) Return a fresh 64-bit symbolic handle in a0 (no extra constraints)
        self._ctx.symbolizeRegister(
            self._ctx.registers.a0, f"create_file_plain_ret_{self._total_executed}"
        )

    def model_create_file_enc(self) -> None:
        # uint64_t create_file_enc([[in]] uint8_t* fbytes, [[in]] uint32_t len, [[in]] uint32_t* iv, [[out]] uint32_t* tag);

        # a0=fbytes, a1=len, a2=iv, a3=tag
        # pylint: disable=unbalanced-tuple-unpacking
        fbytes, fsize, iv_ptr, tag_ptr = self._extract_concrete_args(4)

        logger.debug(
            "create_file_enc(fbytes=0x%016x, len=%u, iv=0x%016x, tag=0x%016x)",
            fbytes,
            fsize,
            iv_ptr,
            tag_ptr,
        )

        # 1. Pointers/len must be non-symbolic
        if self._ctx.isRegisterSymbolized(self._ctx.registers.a0):
            self.reject("create_file_enc: `fbytes` pointer is symbolic!")
        if self._ctx.isRegisterSymbolized(self._ctx.registers.a1):
            self.reject("create_file_enc: `len` is symbolic!")
        if self._ctx.isRegisterSymbolized(self._ctx.registers.a2):
            self.reject("create_file_enc: `iv` pointer is symbolic!")
        if self._ctx.isRegisterSymbolized(self._ctx.registers.a3):
            self.reject("create_file_enc: `tag` pointer is symbolic!")

        if fsize > VerifierConfig.max_file_size:
            self.reject(f"create_file_enc: fsize {fsize} exceeds maximum allowed size!")
        elif fsize % 4 != 0:
            self.reject("create_file_enc: fsize must be multiple of 4!")
        elif fsize == 0:
            self.reject("create_file_enc: fsize cannot be zero!")

        # Verify fbytes is in readable memory and not tainted.
        self._verifier.verify_memory_range(fbytes, fsize)

        # Verify iv is in readable memory and not tainted.
        self._verifier.verify_memory_range(iv_ptr, 12)

        # 2. tag is [[out]]: model as symbolic memory (untaint first)
        for off in range(0, 16, 4):
            self._ctx.untaintMemory(tag_ptr + off)
            self._ctx.setConcreteMemoryValue(MemoryAccess(tag_ptr + off, 4), 0, True)
            self._ctx.symbolizeMemory(
                MemoryAccess(tag_ptr + off, 4),
                f"create_file_enc_out_tag_{off}_{self._total_executed}",
            )

        # 3. Return a fresh 64-bit symbolic handle in a0 (no extra constraints)
        self._ctx.symbolizeRegister(
            self._ctx.registers.a0, f"create_file_enc_ret_{self._total_executed}"
        )

    def model_read_file_plain(self) -> None:
        # int32_t read_file_plain(uint64_t public_handle, [[out]] uint8_t* fbytes, [[int|out]] uint32_t* fsize);

        # pylint: disable=unbalanced-tuple-unpacking
        public_handle, fbytes, fsize_ptr = self._extract_concrete_args(3)
        # Read the concrete value of `*fsize`, and also call hooks to ensure
        # the memory access is verified.
        fsize = self._ctx.getConcreteMemoryValue(MemoryAccess(fsize_ptr, 4), True)
        logger.debug(
            "read_file_plain(0x%016x, 0x%016x, *0x%016x = %d)",
            public_handle,
            fbytes,
            fsize_ptr,
            fsize,
        )

        if fsize > VerifierConfig.max_file_size:
            self.reject(f"read_file_plain: fsize {fsize} exceeds maximum allowed size!")
        elif fsize % 4 != 0:
            self.reject("read_file_plain: fsize must be multiple of 4!")
        elif fsize == 0:
            self.reject("read_file_plain: fsize cannot be zero!")

        # 1. Check `fbytes` pointer is not symbolic, `*fsize` is not symbolic, and
        #    the value of `*fsize` is not symbolic too.
        if self._ctx.isRegisterSymbolized(self._ctx.registers.a1):
            self.reject("read_file_plain: fbytes pointer is symbolic!")
        if self._ctx.isRegisterSymbolized(self._ctx.registers.a2):
            self.reject("read_file_plain: fsize pointer is symbolic!")
        if self._ctx.isMemorySymbolized(MemoryAccess(fsize_ptr, 4)):
            self.reject("read_file_plain: fsize value is symbolic!")

        # *fsize must be non-symbolic, and should not point to tainted memory.
        self._verifier.verify_memory_range(fsize_ptr, 4)

        # 2. Check [`fbytes`, `fbytes + len`) \in writable memory.
        self._verifier.verify_memory_range(fbytes, fsize, MemoryAccessType.WRITE)

        # 3. Model [`fbytes`, `fbytes + len`) as symbolic memory region.
        for i in range(fsize):
            self._ctx.untaintMemory(fbytes + i)
            self._ctx.setConcreteMemoryValue(MemoryAccess(fbytes + i, 1), 0, True)
            self._ctx.symbolizeMemory(
                MemoryAccess(fbytes + i, 1),
                f"read_file_plain_byte_{i}_{self._total_executed}",
            )

        # Model `*fsize` as a new symbolic variable, as it is an in/out parameter.
        self._ctx.untaintMemory(MemoryAccess(fsize_ptr, 4))
        self._ctx.setConcreteMemoryValue(
            MemoryAccess(fsize_ptr, 4), fsize, True
        )  # set concrete to 0 first
        self._ctx.symbolizeMemory(
            MemoryAccess(fsize_ptr, 4),
            f"read_file_plain_out_fsize_{self._total_executed}",
        )

        ast = self._ctx.getAstContext()
        a0 = self._ctx.registers.a0

        # fresh 32-bit symbolic return variable
        ret32_var = self._ctx.newSymbolicVariable(
            32, f"read_file_plain_ret32_{self._total_executed}"
        )
        ret32 = ast.variable(ret32_var)

        # sign-extend to XLEN and assign to a0
        ret_sext = ast.sx(a0.getBitSize() - 32, ret32)
        sexpr = self._ctx.newSymbolicExpression(
            ret_sext, f"read_file_plain_ret_{self._total_executed}"
        )

        self._ctx.setConcreteRegisterValue(a0, 0)
        self._ctx.assignSymbolicExpressionToRegister(sexpr, a0)

        self._ctx.pushPathConstraint(
            ast.equal(ret32, ast.bv(0, 32)),
        )

    def model_read_file_enc(self) -> None:
        # int32_t read_file_enc(uint64_t public_handle, [[in]] uint32_t* auth_tag,
        #                       [[out]] uint8_t* fbytes, [[in|out]] uint32_t* len);

        # a0=handle, a1=auth_tag, a2=fbytes, a3=len
        # pylint: disable=unbalanced-tuple-unpacking
        public_handle, auth_tag_ptr, fbytes, fsize_ptr = self._extract_concrete_args(4)

        # Read concrete *len with hooks so access is verified
        fsize = self._ctx.getConcreteMemoryValue(MemoryAccess(fsize_ptr, 4), True)
        logger.debug(
            "read_file_enc(0x%016x, 0x%016x, 0x%016x, *0x%016x = %d)",
            public_handle,
            auth_tag_ptr,
            fbytes,
            fsize_ptr,
            fsize,
        )

        if fsize > VerifierConfig.max_file_size:
            self.reject(f"read_file_enc: fsize {fsize} exceeds maximum allowed size!")
        elif fsize % 4 != 0:
            self.reject("read_file_enc: fsize must be multiple of 4!")
        elif fsize == 0:
            self.reject("read_file_enc: fsize cannot be zero!")

        # 1. Pointers themselves must be non-symbolic
        if self._ctx.isRegisterSymbolized(self._ctx.registers.a1):
            self.reject("read_file_enc: `auth_tag` pointer is symbolic!")
        if self._ctx.isRegisterSymbolized(self._ctx.registers.a2):
            self.reject("read_file_enc: `fbytes` pointer is symbolic!")
        if self._ctx.isRegisterSymbolized(self._ctx.registers.a3):
            self.reject("read_file_enc: `len` pointer is symbolic!")

        # *len must be non-symbolic
        if self._ctx.isMemorySymbolized(MemoryAccess(fsize_ptr, 4)):
            self.reject("read_file_enc: `*len` value is symbolic!")

        # 1. Verify *len is in readable memory and not tainted.
        self._verifier.verify_memory_range(fsize_ptr, 4)

        # 2. auth_tag is [[in]]: require readable, non-tainted, but can be symbolic.
        self._verifier.verify_memory_range(auth_tag_ptr, 16)

        # 3. Check [`fbytes`, `fbytes + len`) \in writable memory.
        self._verifier.verify_memory_range(fbytes, fsize, MemoryAccessType.WRITE)

        # 4. Model output buffer [fbytes, fbytes+flen) as symbolic bytes (untaint first)
        for i in range(fsize):
            self._ctx.untaintMemory(MemoryAccess(fbytes + i, 1))
            self._ctx.setConcreteMemoryValue(MemoryAccess(fbytes + i, 1), 0, True)
            self._ctx.symbolizeMemory(
                MemoryAccess(fbytes + i, 1),
                f"read_file_enc_byte_{i}_{self._total_executed}",
            )

        # 4. Model *len as out param (symbolic)
        self._ctx.untaintMemory(MemoryAccess(fsize_ptr, 4))
        self._ctx.setConcreteMemoryValue(MemoryAccess(fsize_ptr, 4), fsize, True)
        self._ctx.symbolizeMemory(
            MemoryAccess(fsize_ptr, 4),
            f"read_file_enc_out_len_{self._total_executed}",
        )

        ast = self._ctx.getAstContext()
        a0 = self._ctx.registers.a0

        ret32_var = self._ctx.newSymbolicVariable(
            32, f"read_file_enc_ret32_{self._total_executed}"
        )
        ret32 = ast.variable(ret32_var)
        ret_sext = ast.sx(a0.getBitSize() - 32, ret32)

        self._ctx.setConcreteRegisterValue(a0, 0)
        sexpr = self._ctx.newSymbolicExpression(
            ret_sext, f"read_file_enc_ret_{self._total_executed}"
        )
        self._ctx.assignSymbolicExpressionToRegister(sexpr, a0)
        self._ctx.pushPathConstraint(
            ast.equal(ret32, ast.bv(0, 32)),
        )

    def model_mini_printf(self) -> None:
        # Check format string is concrete and points to readable, non-symbolic memory.

        fmt_ptr, *args = self._extract_concrete_args(8, 0xFFFFFFFFFFFFFFFF)
        fmt = self._get_concrete_memory_string_checked(fmt_ptr)

        logger.debug(
            "mini_printf: fmt='%s', register args=%s",
            fmt.decode().replace("\n", "\\n"),
            args,
        )

        if self._ctx.isRegisterSymbolized(self._ctx.registers.a0):
            self.reject("mini_printf: format string pointer is symbolic!")

        all_format_specifiers = {
            b"%llx": "{:016x}",
            b"%x": "{:08x}",
            b"%d": "{:d}",
            b"%c": "{:c}",
            b"%s": "{}",
            b"%p": "{:016x}",
            b"%b": "{:b}",
        }
        num_specifiers = sum(fmt.count(fs) for fs in all_format_specifiers)

        # Check SP is not symbolic
        sp0 = self._ctx.getConcreteRegisterValue(self._ctx.registers.sp)
        if self._ctx.isRegisterSymbolized(self._ctx.registers.sp):
            self.reject("mini_printf: SP is symbolic, cannot read stack arguments!")

        for stack_argn_n in range(num_specifiers - len(args)):
            cur_arg_loc = sp0 + stack_argn_n * 8
            va_arg = self._ctx.getConcreteMemoryValue(
                MemoryAccess(cur_arg_loc, 8), True
            )
            args.append(va_arg)

        def parse_specs(buf: bytes) -> list[bytes]:
            i, out = 0, []
            while i < len(buf):
                for tok in all_format_specifiers:
                    if buf.startswith(tok, i):
                        out.append(tok)
                        i += len(tok)
                        break
                else:
                    i += 1
            return out

        # Helper: detect symbolic %s pointers from reg or stack
        def arg_is_symbolic_ptr(idx: int) -> bool:
            if idx < 8:
                reg = getattr(self._ctx.registers, f"a{idx}")
                return self._ctx.isRegisterSymbolized(reg)
            slot = sp0 + (idx - 8) * 8
            sym = self._ctx.isMemorySymbolized(slot)
            return sym

        specs = parse_specs(fmt)

        # For every parameter verify it is safe:
        for i, spec in enumerate(specs):
            if spec == b"%x":
                args[i] &= 0xFFFFFFFF
            elif spec == b"%d":
                args[i] = args[i] & 0xFFFFFFFF
                if args[i] & 0x80000000:
                    args[i] -= 0x100000000

            if spec in [b"%s"]:
                # %s: disallow symbolic; otherwise read via checked helper (verifies all bytes)
                if arg_is_symbolic_ptr(i + 1):
                    self.reject(
                        f"mini_printf: `%s` pointer 0x{args[i]:016x} at format specifier {i} is symbolic!"
                    )
                extracted_str = self._get_concrete_memory_string_checked(args[i])
                args[i] = extracted_str.decode("ascii", errors="replace")

        # Finally, replace all format specifiers with safe Python equivalents
        py_fmt = fmt.decode("ascii", errors="ignore").replace("\n", "\\n")
        for spec, py_spec in all_format_specifiers.items():
            py_fmt = py_fmt.replace(spec.decode(), py_spec)

        logger.debug("mini_printf: %s", py_fmt.format(*args))

        self._ctx.setConcreteRegisterValue(self._ctx.registers.a0, 0)
