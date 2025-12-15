from __future__ import annotations
from .base import Pattern, PatternInfo
from ..helpers import random_ascii_string
import random

class GetEncrypted1(Pattern):
    info = PatternInfo(
        name="get_encrypted_1",
        description="Pattern for getting encrypted data.",
        category="get-enc",
    )

    def sample_params(self, rng: random.Random):
        enc_key = hex(int.from_bytes(rng.randbytes(8), 'little'))

        wrong_tag = [rng.randbytes(16)]
        random_iters = rng.randrange(0, 10)
        read_wrong_tag = rng.choice([True, False])

        need_random_prefix = rng.choice([True, False])
        random_prefix_size = rng.randrange(5, 23)
        random_prefix = [rng.randbytes(random_prefix_size)]

        random_handle = rng.randrange(0x0, 0xFFFFFFFF)

        do_inline = rng.choice([True, False])
        decoy = rng.choice([True, False])

        random_int = rng.randrange(0, 256)
        triple_include = rng.choice([True, False])

        enc_handle = rng.randrange(0, 1)
        auth_tag = [rng.randbytes(16)]

        return dict(
            enc_key=enc_key,
            wrong_tag=wrong_tag,
            random_iters=random_iters,
            read_wrong_tag=read_wrong_tag,
            need_random_prefix=need_random_prefix,
            random_prefix_size=random_prefix_size,
            random_prefix=random_prefix,
            random_handle=random_handle,
            do_inline=do_inline,
            random_int=random_int,
            triple_include=triple_include,
            decoy=decoy,
            enc_handle=enc_handle,
            auth_tag=auth_tag
        )

'''
class GetEncrypted1(Pattern):
    info = PatternInfo(
        name="get_encrypted_1",
        description="Pattern for getting encrypted data.",
        category="get-enc",
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
'''
