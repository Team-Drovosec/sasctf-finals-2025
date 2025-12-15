from typing import Dict, Tuple, Final, Mapping

from ..triton_verifier.memory_map import MemoryMap, mm


def test_find_region_top_level():
    region = mm.find_region(0x13370010)
    assert region is not None
    assert region.name == "gatekeeper-mmio"


def test_find_region_subregion():
    region = mm.find_region(0x80001000)
    assert region is not None
    assert region.name == "create_file_plain"


def test_permission_checks():
    code_region = mm.find_region_by_name("user-code")
    assert code_region is not None
    assert mm.is_execute_allowed(code_region.start)
    assert not mm.is_write_allowed(code_region.start)

    data_region = mm.find_region_by_name("user-data")
    assert data_region is not None
    assert mm.is_read_allowed(data_region.start)
    assert mm.is_write_allowed(data_region.start)
    assert not mm.is_execute_allowed(data_region.start)


def test_memory_map_custom_instance():
    _MEMORY_MAP_RAW: Final[
        Mapping[
            str,
            tuple[int, int, str] | tuple[int, int, str, Mapping[str, tuple[int, int]]],
        ]
    ] = {
        "gatekeeper-mmio": (0x13370000, 0x13370000 + 0x1000, "---"),
        "handles-table": (0x13400000, 0x13400000 + 0x20000, "---"),
        "uart": (0xFFFF0000, 0xFFFF0000 + 0x1000, "rw-"),  # @TODO!
        "trusted-funcs": (
            0x80000000,
            0x80000000 + 0x10000,
            "r-x",
            {
                "create_file_plain": (0x80000000, 0x80000000 + 0x2000),
                "create_file_enc": (0x80002000, 0x80002000 + 0x2000),
                "read_file_plain": (0x80004000, 0x80004000 + 0x2000),
                "read_file_enc": (0x80006000, 0x80006000 + 0x2000),
            },
        ),
        "user-code": (0x80010000, 0x80010000 + 0x10000, "r-x"),
        "user-data": (0x80020000, 0x80020000 + 0x10000, "rw-"),
        "stack": (0x80030000, 0x80030000 + 0x20000, "rw-"),
    }

    custom_mm = MemoryMap.from_layout(_MEMORY_MAP_RAW)
    region = custom_mm.find_region(0x80001000)
    assert region is not None
    assert region.name == "create_file_plain"

    region = custom_mm.find_region(0x13400010)
    assert region is not None
    assert region.name == "handles-table"

    region = custom_mm.find_region(0x12345678)
    assert region is None

    code_region = custom_mm.find_region_by_name("user-code")
    assert code_region is not None
    assert custom_mm.is_execute_allowed(code_region.start)
    assert not custom_mm.is_write_allowed(code_region.start)

    data_region = custom_mm.find_region_by_name("user-data")
    assert data_region is not None
    assert custom_mm.is_read_allowed(data_region.start)
    assert custom_mm.is_write_allowed(data_region.start)
    assert not custom_mm.is_execute_allowed(data_region.start)

    assert custom_mm.find_region_by_name("non-existent") is None
    assert len(list(custom_mm.iter_all_regions())) == 11
