from __future__ import annotations
from .base import Pattern, PatternInfo
import random


class MonoTrustMiniTainted(Pattern):
    info = PatternInfo(
        name="mono_trust_mini_tainted",
        description="Monolithic template on ministd with one guaranteed attempt to read tainted memory (reject).",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        count_plain = rng.randint(1, 4)
        count_enc = rng.randint(0, 3)
        sizes = [4, 16, 64, 128]
        file_size = rng.choice(sizes)
        format_case = rng.randint(0, 1)

        taint_kind = rng.randint(0, 2)
        if taint_kind == 0:
            taint_size = rng.choice([8, 16])
            return dict(
                COUNT_PLAIN=count_plain,
                COUNT_ENC=count_enc,
                FILE_SIZE=file_size,
                FORMAT_CASE=format_case,
                TAINT_KIND=taint_kind,
                TAINT_SIZE=taint_size,
            )
        elif taint_kind == 1:
            words = rng.choice([2, 4])
            return dict(
                COUNT_PLAIN=count_plain,
                COUNT_ENC=count_enc,
                FILE_SIZE=file_size,
                FORMAT_CASE=format_case,
                TAINT_KIND=taint_kind,
                TAINT_SIZE_WORDS=words,
            )
        else:
            qwords = 2
            return dict(
                COUNT_PLAIN=count_plain,
                COUNT_ENC=count_enc,
                FILE_SIZE=file_size,
                FORMAT_CASE=format_case,
                TAINT_KIND=taint_kind,
                TAINT_SIZE_QWORDS=qwords,
            )


class ModuTrustMiniTainted(Pattern):
    info = PatternInfo(
        name="modu_trust_mini_tainted",
        description="Modular template with one guaranteed attempt to read tainted memory (reject).",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        count_plain = rng.randint(1, 4)
        sizes = [4, 16, 64, 128]
        file_size = rng.choice(sizes)
        format_case = rng.randint(0, 1)

        taint_kind = rng.randint(0, 2)
        if taint_kind == 0:
            taint_size = rng.choice([8, 16])
            return dict(
                COUNT_PLAIN=count_plain,
                FILE_SIZE=file_size,
                FORMAT_CASE=format_case,
                TAINT_KIND=taint_kind,
                TAINT_SIZE=taint_size,
            )
        elif taint_kind == 1:
            words = rng.choice([2, 4])  # 32-bit words
            return dict(
                COUNT_PLAIN=count_plain,
                FILE_SIZE=file_size,
                FORMAT_CASE=format_case,
                TAINT_KIND=taint_kind,
                TAINT_SIZE_WORDS=words,
            )
        else:
            qwords = 2  # 64-bit qwords
            return dict(
                COUNT_PLAIN=count_plain,
                FILE_SIZE=file_size,
                FORMAT_CASE=format_case,
                TAINT_KIND=taint_kind,
                TAINT_SIZE_QWORDS=qwords,
            )


class MonoMiniTainted(Pattern):
    info = PatternInfo(
        name="mono_mini_tainted",
        description="Monolithic template on ministd with one guaranteed attempt to read tainted memory (reject).",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        sizes = [4, 16, 64, 128]
        file_size = rng.choice(sizes)

        taint_kind = rng.randint(0, 2)
        if taint_kind == 0:
            taint_size = rng.choice([8, 16])
            return dict(
                FILE_SIZE=file_size,
                TAINT_KIND=taint_kind,
                TAINT_SIZE=taint_size,
            )
        elif taint_kind == 1:
            words = rng.choice([2, 4])  # 32-bit words
            return dict(
                FILE_SIZE=file_size,
                TAINT_KIND=taint_kind,
                TAINT_SIZE_WORDS=words,
            )
        else:
            qwords = 2  # 64-bit qwords
            return dict(
                FILE_SIZE=file_size,
                TAINT_KIND=taint_kind,
                TAINT_SIZE_QWORDS=qwords,
            )


class ModuMiniTainted(Pattern):
    info = PatternInfo(
        name="modu_mini_tainted",
        description="Modular template on ministd with one guaranteed attempt to read tainted memory (reject).",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        sizes = [4, 8, 16, 32, 64, 128, 256]
        file_size = rng.choice(sizes)

        taint_kind = rng.randint(0, 2)
        if taint_kind == 0:
            taint_size = 8
            return dict(
                FILE_SIZE=file_size,
                TAINT_KIND=taint_kind,
                TAINT_SIZE=taint_size,
            )
        elif taint_kind == 1:
            words = 2  # 32-bit words
            return dict(
                FILE_SIZE=file_size,
                TAINT_KIND=taint_kind,
                TAINT_SIZE_WORDS=words,
            )
        else:
            qwords = 8  # 64-bit qwords
            return dict(
                FILE_SIZE=file_size,
                TAINT_KIND=taint_kind,
                TAINT_SIZE_QWORDS=qwords,
            )


class MonoTrustTainted(Pattern):
    info = PatternInfo(
        name="mono_trust_tainted",
        description="Monolithic template on trusted_lib_api with one guaranteed attempt to read tainted memory (reject).",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        count_plain = rng.randint(1, 4)
        count_enc = rng.randint(0, 3)
        sizes = [4, 16, 64, 128]
        file_size = rng.choice(sizes)
        format_case = rng.randint(0, 1)

        taint_kind = rng.randint(0, 2)
        if taint_kind == 0:
            taint_size = rng.choice(
                [file_size + 8, file_size + 16, file_size + 32]
            )
            return dict(
                COUNT_PLAIN=count_plain,
                COUNT_ENC=count_enc,
                FILE_SIZE=file_size,
                FORMAT_CASE=format_case,
                TAINT_KIND=taint_kind,
                TAINT_SIZE=taint_size,
            )
        elif taint_kind == 1:
            words = rng.choice([file_size // 4 + 2, file_size // 4 + 4])
            return dict(
                COUNT_PLAIN=count_plain,
                COUNT_ENC=count_enc,
                FILE_SIZE=file_size,
                FORMAT_CASE=format_case,
                TAINT_KIND=taint_kind,
                TAINT_SIZE_WORDS=words,
            )
        else:
            qwords = file_size // 8 + 2
            return dict(
                COUNT_PLAIN=count_plain,
                COUNT_ENC=count_enc,
                FILE_SIZE=file_size,
                FORMAT_CASE=format_case,
                TAINT_KIND=taint_kind,
                TAINT_SIZE_QWORDS=qwords,
            )


class ModuTrustTainted(Pattern):
    info = PatternInfo(
        name="modu_trust_tainted",
        description="Modular template on trusted_lib_api with one guaranteed attempt to read tainted memory (reject).",
        category="reject",
    )

    def sample_params(self, rng: random.Random):
        count_plain = rng.randint(1, 4)
        sizes = [4, 16, 64, 128, 256]
        file_size = rng.choice(sizes)
        format_case = rng.randint(0, 1)

        taint_kind = rng.randint(0, 2)
        if taint_kind == 0:
            taint_size = rng.choice([file_size + 8, file_size + 16, file_size + 32])
            return dict(
                COUNT_PLAIN=count_plain,
                FILE_SIZE=file_size,
                FORMAT_CASE=format_case,
                TAINT_KIND=taint_kind,
                TAINT_SIZE=taint_size,
            )
        elif taint_kind == 1:
            words = rng.choice([file_size // 4 + 2, file_size // 4 + 4])
            return dict(
                COUNT_PLAIN=count_plain,
                FILE_SIZE=file_size,
                FORMAT_CASE=format_case,
                TAINT_KIND=taint_kind,
                TAINT_SIZE_WORDS=words,
            )
        else:
            qwords = file_size // 8 + 2
            return dict(
                COUNT_PLAIN=count_plain,
                FILE_SIZE=file_size,
                FORMAT_CASE=format_case,
                TAINT_KIND=taint_kind,
                TAINT_SIZE_QWORDS=qwords,
            )
