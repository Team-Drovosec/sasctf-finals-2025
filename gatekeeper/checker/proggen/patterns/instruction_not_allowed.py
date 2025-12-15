from __future__ import annotations
import random
from .base import Pattern, PatternInfo


class MonoTrustMiniRejectIllegalInstr(Pattern):
    info = PatternInfo(
        name="mono_trust_mini_reject_illegal_instr_rand",
        description="Inject a randomly chosen illegal instruction bytes sequence at variable places -> reject: instruction not allowed",
        category="reject",
    )

    ILLEGAL_BYTE_SEQS = [
        [0xA9, 0xCB],  # c.beqz
        [0x00, 0x00, 0x00, 0x00],  # c.unimp
        [0x44, 0x33, 0x22, 0x11],  # c.fld
        [0xDE, 0xAD, 0xBE, 0xEF],  # c.fsdsp
    ]

    def sample_params(self, rng: random.Random):
        file_size = rng.choice([64, 128, 256])
        count_plain = rng.randint(2, 4)
        count_enc = rng.randint(1, 3)
        format_case = rng.randint(0, 1)
        seed = rng.randint(0, 10000)

        seq = rng.choice(self.ILLEGAL_BYTE_SEQS)
        byte_str = ",".join("0x{:02x}".format(b) for b in seq)

        return dict(
            FILE_SIZE=file_size,
            COUNT_PLAIN=count_plain,
            COUNT_ENC=count_enc,
            FORMAT_CASE=format_case,
            seed=seed,
            ILLEGAL_INSTRUCTION_BYTES=byte_str,
            pattern_name=self.info.name,
        )


class ModuTrustMiniRejectIllegalInstr(Pattern):
    info = PatternInfo(
        name="modu_trust_mini_reject_illegal_instr",
        description="Modular template with trusted API, causing rejection due to unsupported instructions (illegal instr).",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        file_size = rng.choice([64, 128, 256])
        count_plain = rng.randint(2, 4)
        format_case = rng.randint(0, 1)
        seed = rng.randint(0, 10000)

        illegal_seqs = [
            [0xA9, 0xCB],  # c.beqz
            [0x00, 0x00, 0x00, 0x00],  # c.unimp
            [0x44, 0x33, 0x22, 0x11],  # c.fld
            [0xDE, 0xAD, 0xBE, 0xEF],  # c.fsdsp
        ]
        seq = rng.choice(illegal_seqs)
        illegal_bytes = ", ".join(f"0x{b:02X}" for b in seq)

        return dict(
            FILE_SIZE=file_size,
            COUNT_PLAIN=count_plain,
            FORMAT_CASE=format_case,
            seed=seed,
            ILLEGAL_INSTRUCTION_BYTES=illegal_bytes,
        )


class MonoMiniRejectIllegalInstr(Pattern):
    info = PatternInfo(
        name="mono_mini_reject_illegal_instr",
        description="Monolithic template on ministd with unsupported instructions causing rejection (illegal instr).",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        file_size = rng.choice([32, 64, 128, 256])
        seed = rng.randint(0, 10000)

        illegal_seqs = [
            [0xA9, 0xCB],  # c.beqz
            [0x00, 0x00, 0x00, 0x00],  # c.unimp
            [0x44, 0x33, 0x22, 0x11],  # c.fld
            [0xDE, 0xAD, 0xBE, 0xEF],  # c.fsdsp
        ]
        seq = rng.choice(illegal_seqs)
        illegal_bytes = ", ".join(f"0x{b:02X}" for b in seq)

        return dict(
            FILE_SIZE=file_size,
            seed=seed,
            ILLEGAL_INSTRUCTION_BYTES=illegal_bytes,
        )


class ModuMiniRejectIllegalInstr(Pattern):
    info = PatternInfo(
        name="modu_mini_reject_illegal_instr",
        description="Modular template on ministd with unsupported instructions causing rejection (illegal instr).",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        file_size = rng.choice([32, 64, 128, 256])
        seed = rng.randint(0, 10000)

        illegal_seqs = [
            [0xA9, 0xCB],  # c.beqz
            [0x00, 0x00, 0x00, 0x00],  # c.unimp
            [0x44, 0x33, 0x22, 0x11],  # c.fld
            [0xDE, 0xAD, 0xBE, 0xEF],  # c.fsdsp
        ]
        seq = rng.choice(illegal_seqs)
        illegal_bytes = ", ".join(f"0x{b:02X}" for b in seq)

        return dict(
            FILE_SIZE=file_size,
            seed=seed,
            ILLEGAL_INSTRUCTION_BYTES=illegal_bytes,
        )


class MonoTrustRejectIllegalInstr(Pattern):
    info = PatternInfo(
        name="mono_trust_reject_illegal_instr",
        description="Monolithic template on trusted_lib_api with unsupported instructions causing rejection (illegal instr).",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        file_size = rng.choice([64, 128, 256])
        count_plain = rng.randint(1, 3)
        count_enc = rng.randint(1, 3)
        format_case = rng.randint(0, 1)
        seed = rng.randint(0, 10000)

        illegal_seqs = [
            [0xA9, 0xCB],  # c.beqz
            [0x00, 0x00, 0x00, 0x00],  # c.unimp
            [0x44, 0x33, 0x22, 0x11],  # c.fld
            [0xDE, 0xAD, 0xBE, 0xEF],  # c.fsdsp
        ]
        seq = rng.choice(illegal_seqs)
        illegal_bytes = ", ".join(f"0x{b:02X}" for b in seq)

        return dict(
            FILE_SIZE=file_size,
            COUNT_PLAIN=count_plain,
            COUNT_ENC=count_enc,
            FORMAT_CASE=format_case,
            seed=seed,
            ILLEGAL_INSTRUCTION_BYTES=illegal_bytes,
        )


class ModuTrustRejectIllegalInstr(Pattern):
    info = PatternInfo(
        name="modu_trust_reject_illegal_instr",
        description="Modular template with trusted API, causing rejection due to unsupported instructions (illegal instr).",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        file_size = rng.choice([64, 128, 256])
        count_plain = rng.randint(1, 4)
        format_case = rng.randint(0, 1)
        seed = rng.randint(0, 10000)

        illegal_seqs = [
            [0xA9, 0xCB],  # c.beqz
            [0x00, 0x00, 0x00, 0x00],  # c.unimp
            [0x44, 0x33, 0x22, 0x11],  # c.fld
            [0xDE, 0xAD, 0xBE, 0xEF],  # c.fsdsp
        ]
        seq = rng.choice(illegal_seqs)
        illegal_bytes = ", ".join(f"0x{b:02X}" for b in seq)

        return dict(
            FILE_SIZE=file_size,
            COUNT_PLAIN=count_plain,
            FORMAT_CASE=format_case,
            seed=seed,
            ILLEGAL_INSTRUCTION_BYTES=illegal_bytes,
        )
