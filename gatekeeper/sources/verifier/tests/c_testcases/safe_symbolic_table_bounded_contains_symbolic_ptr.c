#include "trusted_lib_api.h"

static uint8_t g_Data[128] = { 0 };

uint64_t align_up(uint64_t addr, uint64_t align)
{
    return (addr + align - 1) & ~(align - 1);
}

__attribute__((noinline)) void safe_symbolic_table_bounded_contains_symbolic_ptr()
{
    uint8_t data[16];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);

    char* symbolic_ptr = (char*)((uint64_t)(*(volatile char**)&data[0]) ^ (uint64_t)g_Data);
    symbolic_ptr = (char*)verifier_ranged_u32(
        (uint64_t)symbolic_ptr, (uint64_t)g_Data - 4, (uint64_t)g_Data + 32);
    symbolic_ptr = (char*)align_up((uint64_t)symbolic_ptr, 4);
    uint32_t symbolic_var = verifier_ranged_u32(*(volatile uint32_t*)&data[4], 0, 2);

    const uint32_t* ptrs[] = { (void*)g_Data,     (void*)symbolic_ptr, (void*)g_Data,
                               (void*)0x41414141, (void*)0x42424242,   (void*)0x43434343 };
    const uint32_t* p = ptrs[symbolic_var];
    [[maybe_unused]] const volatile uint32_t v =
        *(volatile uint32_t*)p; // this one should be marked as symbolic, and
                                // safe.
}

ENTRY_POINT_ATTR void start()
{
    safe_symbolic_table_bounded_contains_symbolic_ptr();
    ENTRY_POINT_END();
}
