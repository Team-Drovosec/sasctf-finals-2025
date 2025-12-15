from pathlib import Path

NUM_ROUNDS = 250

PROJECT_PATH = Path(__file__).resolve().parent.parent
BASE_GENERATED_PROGRAMS = PROJECT_PATH / "checker" / "generated" / "base"


class SimpleProgramGenerator:
    _num_rounds: int
    _storage_put_plain: bytes = bytes()
    _storage_put_enc: bytes = bytes()
    _storage_get_plain: bytes = bytes()
    _storage_get_enc: bytes = bytes()

    def __init__(self, num_rounds: int = NUM_ROUNDS):
        self._num_rounds = num_rounds

    def set_storage(self, field: str, path: Path) -> None:
        exec(f'self._storage_{field} = path.open("rb").read()')

    def get_storage(
        self,
        field: str,
        handle: bytes = None,
        iv: bytes = None,
        tag: bytes = None,
        flag: bytes = None,
    ) -> bytes:
        tmp = eval(f"self._storage_{field}")
        if handle is not None:
            tmp = tmp.replace(b"\xbb" * 8, handle)

        if iv is not None:
            tmp = tmp.replace(b"\xff" * 12, iv)

        if tag is not None:
            tmp = tmp.replace(b"\xcc" * 16, tag)

        if flag is not None:
            tmp = tmp.replace(b"\xaa" * 32, flag)

        return tmp


def init_generator() -> SimpleProgramGenerator:
    generator = SimpleProgramGenerator()
    generator.set_storage("get_plain", BASE_GENERATED_PROGRAMS / "get_plain.bin")
    generator.set_storage("get_enc", BASE_GENERATED_PROGRAMS / "get_enc.bin")
    generator.set_storage("put_plain", BASE_GENERATED_PROGRAMS / "put_plain.bin")
    generator.set_storage("put_enc", BASE_GENERATED_PROGRAMS / "put_enc.bin")

    return generator
