"""
@Author: madrat
Whitelist of allowed RISC-V instructions for the Triton verifier.
"""


class InstructionsWhitelist:
    # RV32I, RV64I Instructions
    _RV32I_RV64I = {
        "lui",
        "auipc",
        "addi",
        "slti",
        "sltiu",
        "xori",
        "ori",
        "andi",
        "slli",
        "srli",
        "srai",
        "add",
        "sub",
        "sll",
        "slt",
        "sltu",
        "xor",
        "srl",
        "sra",
        "or",
        "and",
        "ebreak",
        "lb",
        "lh",
        "lw",
        "lbu",
        "lhu",
        "sb",
        "sh",
        "sw",
        "jal",
        "jalr",
        "ret",
        "beq",
        "bne",
        "blt",
        "bge",
        "bltu",
        "bgeu",
    }

    # RV64I Instructions
    _RV64I = {
        "addiw",
        "slliw",
        "srliw",
        "sraiw",
        "addw",
        "subw",
        "sllw",
        "srlw",
        "sraw",
        "lwu",
        "ld",
        "sd",
    }

    # RV32M, RV64M Instructions
    _RV32M_RV64M = {
        "mul",
        "mulh",
        "mulhsu",
        "mulhu",
        "div",
        "divu",
        "rem",
        "remu",
    }

    # RV64M Instructions
    _RV64M = {
        "mulw",
        "divw",
        "divuw",
        "remw",
        "remuw",
    }

    _PSEUDO = {
        "nop",  # addi x0, x0, 0
        "li",  # lui + addi
        "mv",  # addi rd, rs, 0
        "not",  # xori rd, rs, -1
        "neg",  # sub rd, x0, rs
        "negw",  # subw rd, x0, rs
        "sext.w",  # addiw rd, rs, 0
        "seqz",  # sltiu rd, rs, 1
        "snez",  # sltu rd, x0, rs
        "sltz",  # slt rd, rs, x0
        "sgtz",  # slt rd, x0, rs
        # Branch instructions
        "beqz",  # beq rs, x0, offset
        "bnez",  # bne rs, x0, offset
        "blez",  # bge x0, rs, offset
        "bgez",  # bge rs, x0, offset
        "bltz",  # blt rs, x0, offset
        "bgtz",  # blt x0, rs, offset
        # Branch instructions
        "bgt",  # blt rt, rs, offset
        "ble",  # bge rt, rs, offset
        "bgtu",  # bltu rt, rs, offset
        "bleu",  # bgeu rt, rs, offset
        # Jump instructions
        "j",  # jal x0, offset
        "jal",  # jal x1, offset
        "jr",  # jalr x0, rs, 0
        "jalr",  # jalr x1, rs, 0
        "ret",  # jalr x0, x1, 0
        "call",  # auipc x1, offset[31:12]; jalr x1, x1, offset[11:0]
    }

    _ALLOWED_INSTRUCTIONS = {
        *_RV32I_RV64I,
        *_RV64I,
        *_RV32M_RV64M,
        *_RV64M,
        *_PSEUDO,
    }

    def is_allowed(self, mnemonic: str) -> bool:
        mnemonic = mnemonic.lower().split(" ")[0].strip()
        return mnemonic in self._ALLOWED_INSTRUCTIONS

    def __contains__(self, mnemonic: str) -> bool:
        return self.is_allowed(mnemonic)


whitelist = InstructionsWhitelist()
