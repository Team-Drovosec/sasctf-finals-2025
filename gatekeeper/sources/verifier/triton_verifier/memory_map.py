"""
@Author: madrat
In-memory representation of the Gatekeeper's memory map.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Final, Mapping, Tuple

_MEMORY_MAP_RAW: Final[
    Mapping[
        str, tuple[int, int, str] | tuple[int, int, str, Mapping[str, tuple[int, int]]]
    ]
] = {
    "gatekeeper-mmio": (0x13370000, 0x13370000 + 0x1000, "---"),
    "handles-table": (0x13400000, 0x13400000 + 0x80000, "---"),
    "trusted-funcs": (
        0x80000000,
        0x80000000 + 0x10000,
        "--x",
        {
            # uint64_t create_file_plain([[in]] uint8_t* fbytes, [[in]] uint32_t len);
            "create_file_plain": (0x80000000, 0x80000000 + 0x2000),
            # uint64_t create_file_enc([[in]] uint8_t* fbytes, [[in]] uint32_t len, [[in]] uint32_t* iv, [[out]] uint32_t* tag);
            "create_file_enc": (0x80002000, 0x80002000 + 0x2000),
            # int32_t read_file_plain(uint64_t public_handle, [[out]] uint8_t* fbytes, [[in|out]] uint32_t* len);
            "read_file_plain": (0x80004000, 0x80004000 + 0x2000),
            # int32_t read_file_enc(uint64_t public_handle, [[in]] uint32_t* auth_tag, [[out]] uint8_t* fbytes, [[in|out]] uint32_t* len);
            "read_file_enc": (0x80006000, 0x80006000 + 0x2000),
            # mini_printf(const char* fmt, ...)
            "mini_printf": (0x8000A000, 0x8000A000 + 0x1000),
        },
    ),
    "user-code": (0x300000, 0x300000 + 0x10000, "r-x"),
    "user-data": (0x310000, 0x310000 + 0x10000, "rw-"),
    "stack": (0x320000, 0x320000 + 0x20000, "rw-"),
}


@dataclass(slots=True, frozen=True)
class MemoryRegion:
    name: str
    start: int
    end: int
    permissions: str
    subregions: Tuple["MemoryRegion", ...] = field(default_factory=tuple)

    def contains(self, address: int) -> bool:
        return self.start <= address < self.end

    def size(self) -> int:
        return self.end - self.start

    def __repr__(self) -> str:
        if not self.subregions:
            return (
                f"MemoryRegion(name='{self.name}', start=0x{self.start:X}, "
                f"end=0x{self.end:X}, permissions='{self.permissions}')"
            )

        return (
            f"MemoryRegion(name='{self.name}', start=0x{self.start:X}, "
            f"end=0x{self.end:X}, permissions='{self.permissions}',"
            f"{self.subregions})"
        )


@dataclass(slots=True, frozen=True)
class MemoryMap:
    regions: Tuple[MemoryRegion, ...]

    @classmethod
    def from_layout(
        cls,
        layout: Mapping[
            str,
            tuple[int, int, str] | tuple[int, int, str, Mapping[str, tuple[int, int]]],
        ],
    ) -> "MemoryMap":
        regions: list[MemoryRegion] = []
        for name, spec in layout.items():
            start, end, permissions, *maybe_sub = spec  # type: ignore[misc]
            permissions = permissions or "---"
            subregions = ()
            if maybe_sub:
                sub_dict = maybe_sub[0] or {}
                subregions = tuple(
                    MemoryRegion(sub_name, s, e, permissions)
                    for sub_name, (s, e) in sub_dict.items()
                )
            regions.append(MemoryRegion(name, start, end, permissions, subregions))
        return cls(tuple(regions))

    def _find_region_recursive(
        self, address: int, regions: Tuple[MemoryRegion, ...]
    ) -> MemoryRegion | None:
        for region in regions:
            if region.contains(address):
                # Check subregions first
                subregion = self._find_region_recursive(address, region.subregions)
                return subregion if subregion else region
        return None

    def find_region(self, address: int) -> MemoryRegion | None:
        """
        Recursively search for the deepest memory region containing the given address.
        """
        return self._find_region_recursive(address, self.regions)

    def _find_region_by_name_recursive(
        self, name: str, regions: Tuple[MemoryRegion, ...]
    ) -> MemoryRegion | None:
        for region in regions:
            if region.name == name:
                return region
            subregion = self._find_region_by_name_recursive(name, region.subregions)
            if subregion:
                return subregion
        return None

    def find_region_by_name(self, name: str) -> MemoryRegion | None:
        return self._find_region_by_name_recursive(name, self.regions)

    def is_execute_allowed(self, address: int) -> bool:
        region = self.find_region(address)
        if region and "x" in region.permissions:
            return True
        return False

    def is_read_allowed(self, address: int) -> bool:
        region = self.find_region(address)
        if region and "r" in region.permissions:
            return True
        return False

    def is_write_allowed(self, address: int) -> bool:
        region = self.find_region(address)
        if region and "w" in region.permissions:
            return True
        return False

    def iter_all_regions(self):
        stack = list(self.regions)
        while stack:
            r = stack.pop()
            yield r
            stack.extend(r.subregions)

    def get_executable_regions(self) -> Tuple[MemoryRegion, ...]:
        return tuple(r for r in self.iter_all_regions() if "x" in r.permissions)

    def get_readable_regions(self) -> Tuple[MemoryRegion, ...]:
        return tuple(r for r in self.iter_all_regions() if "r" in r.permissions)

    def get_writable_regions(self) -> Tuple[MemoryRegion, ...]:
        return tuple(r for r in self.iter_all_regions() if "w" in r.permissions)

    def __repr__(self) -> str:
        return f"MemoryMap(regions={self.regions})"


mm: Final[MemoryMap] = MemoryMap.from_layout(_MEMORY_MAP_RAW)

assert mm.find_region_by_name("user-code") is not None
assert mm.find_region_by_name("user-data") is not None
assert mm.find_region_by_name("stack") is not None
assert mm.find_region_by_name("trusted-funcs") is not None
assert mm.find_region_by_name("handles-table") is not None
assert mm.find_region_by_name("gatekeeper-mmio") is not None
assert (
    mm.find_region_by_name("user-code").end == mm.find_region_by_name("user-data").start
)
assert mm.find_region_by_name("user-data").end == mm.find_region_by_name("stack").start
