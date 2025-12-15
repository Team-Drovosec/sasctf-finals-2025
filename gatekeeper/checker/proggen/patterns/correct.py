from __future__ import annotations
from .base import Pattern, PatternInfo
from ..helpers import random_ascii_string
import random


class SimilarToExploitVerifyReturnValue(Pattern):
    info = PatternInfo(
        name="similar_to_exploit_verify_return_value",
        description="Pattern similar to exploit that verifies return value handling.",
        category="accept",
    )

    def sample_params(self, rng: random.Random):
        random_vals = [rng.randrange(0, 0xFFFFFFFF), rng.randrange(0, 0xFFFFFFFF)]
        file_size = rng.choice([64, 128, 256, 512])
        file_data_init = [rng.randrange(0, 0xFF) for _ in range(file_size)]
        test_encrypted = rng.choice([True, False])
        invalid_first = rng.choice([True, False])
        random_handle = rng.randrange(0x1000000000000000, 0xFFFFFFFFFFFFFFFF)
        return dict(
            random_vals=random_vals,
            file_size=file_size,
            file_data_init=file_data_init,
            test_encrypted=test_encrypted,
            invalid_first=invalid_first,
            random_handle=random_handle,
        )


class SimilarToExploitPrintf(Pattern):
    info = PatternInfo(
        name="similar_to_exploit_printf",
        description="Pattern similar to exploit that uses printf-like functionality.",
        category="accept",
    )

    def sample_params(self, rng: random.Random):
        def generate_format_string_and_args():
            num_args = rng.randint(2, 20)
            format_specifiers = []
            args = []
            for _ in range(num_args):
                spec = rng.choice(["%d", "%x", "%s", "%llx", "%b", "%c", "%p"])
                format_specifiers.append(spec)
                if spec == "%d":
                    args.append(str(rng.randint(-0xFFFFFFFF, 0xFFFFFFFF)))
                elif spec == "%x":
                    args.append(hex(rng.randint(0, 0xFFFFFFFF)))
                elif spec == "%s":
                    str_length = rng.randint(3, 40)
                    rand_str = random_ascii_string(rng, 1, str_length)
                    args.append(f'"{rand_str}"')
                elif spec == "%llx":
                    args.append(hex(rng.randint(0, 0xFFFFFFFFFFFFFFFF)))
                elif spec == "%b":
                    args.append(bin(rng.randint(0, 0xFFFF)))
                elif spec == "%c":
                    args.append(f"{rng.randint(0, 255)}")
                elif spec == "%p":
                    args.append(hex(rng.randrange(0x0, 0xFFFFFFFFFFFFFFFF)))

            format_string = ", ".join(format_specifiers) + "\\n"
            return format_string, args

        format_string, format_args = generate_format_string_and_args()

        return {
            "format_string": format_string,
            "format_args": format_args,
        }
