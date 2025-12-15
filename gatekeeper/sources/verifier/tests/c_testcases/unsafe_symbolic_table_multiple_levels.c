#include "trusted_lib_api.h"

static uint8_t g_Data[128] = { 0 };

__attribute__((noinline)) void unsafe_symbolic_table_multiple_levels()
{
    uint8_t data[32];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);

    uint32_t symbolic_index_outer = verifier_ranged_u32(*(volatile uint32_t*)&data[0], 0, 3);

    const uint32_t inner_indices[] = { verifier_ranged_u32(*(volatile uint32_t*)&data[4], 0, 2),
                                       verifier_ranged_u32(*(volatile uint32_t*)&data[8], 0, 2),
                                       verifier_ranged_u32(*(volatile uint32_t*)&data[12], 0, 3),
                                       verifier_ranged_u32(*(volatile uint32_t*)&data[16], 2, 4) };

    const uint32_t* ptrs[] = { (void*)g_Data,    (void*)g_Data,     (void*)g_Data,
                               (void*)g_Data,    (void*)0x41414141, (void*)0x42424242,
                               (void*)0x43434343 };

    const uint32_t inner_index = inner_indices[symbolic_index_outer];
    const uint32_t* p = ptrs[inner_index];
    [[maybe_unused]] const volatile uint32_t v =
        *(volatile uint32_t*)p; // this one should be marked as symbolic, and
                                // unsafe!
}

ENTRY_POINT_ATTR void start()
{
    unsafe_symbolic_table_multiple_levels();
    ENTRY_POINT_END();
}
