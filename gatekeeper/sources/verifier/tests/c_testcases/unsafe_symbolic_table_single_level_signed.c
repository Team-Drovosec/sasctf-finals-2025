#include "trusted_lib_api.h"

static uint8_t g_Data[128] = { 0 };

__attribute__((noinline)) void unsafe_symbolic_table_single_level_signed()
{
    uint8_t data[32];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);

    int32_t sym_idx = -verifier_ranged_u32(*(volatile uint32_t*)&data[0], 3, 4);

    const uint32_t* ptrs[] = {
        (void*)0x41414141, (void*)0x42424242, (void*)0x43434343, (void*)g_Data,
        (void*)g_Data,     (void*)g_Data,     (void*)g_Data,
    };

    const uint32_t* p = ptrs[6 + sym_idx];
    [[maybe_unused]] const volatile uint32_t v =
        *(volatile uint32_t*)p; // this one should be marked as symbolic, and
                                // unsafe!
}

ENTRY_POINT_ATTR void start()
{
    unsafe_symbolic_table_single_level_signed();
    ENTRY_POINT_END();
}
