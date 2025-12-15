#include "trusted_lib_api.h"

static uint8_t g_Data[128] = { 0 };

__attribute__((noinline)) void safe_symbolic_table_multiple_levels()
{
    uint8_t data[20];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);

    uint32_t symbolic_index_outer = verifier_ranged_u32(*(volatile uint32_t*)&data[0], 0, 3);

    const uint32_t inner_indices[] = { verifier_ranged_u32(*(volatile uint32_t*)&data[4], 0, 0),
                                       verifier_ranged_u32(*(volatile uint32_t*)&data[8], 2, 4),
                                       verifier_ranged_u32(*(volatile uint32_t*)&data[12], 2, 3),
                                       verifier_ranged_u32(*(volatile uint32_t*)&data[16], 6, 6) };

    const uint32_t* ptrs[] = { (void*)g_Data, (void*)0x41414141, (void*)g_Data, (void*)g_Data,
                               (void*)g_Data, (void*)0x42424242, (void*)g_Data };

    const uint32_t inner_index = inner_indices[symbolic_index_outer];
    const uint32_t* p = ptrs[inner_index];
    [[maybe_unused]] const volatile uint32_t v =
        *(volatile uint32_t*)p; // this one should be marked as symbolic, and
                                // safe.
}

ENTRY_POINT_ATTR void start()
{
    safe_symbolic_table_multiple_levels();
    ENTRY_POINT_END();
}
