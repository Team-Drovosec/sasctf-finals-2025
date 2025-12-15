#include "trusted_lib_api.h"

static uint8_t g_Data[128] = { 0 };

__attribute__((noinline)) void unsafe_symbolic_table_multiple_levels_signed()
{
    uint8_t data[32];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);

    int32_t symbolic_index_outer = -verifier_ranged_u32(*(volatile uint32_t*)&data[0], 1, 2);
    int32_t symbolic_index_inner = -verifier_ranged_u32(*(volatile uint32_t*)&data[4], 1, 2);
    const uint32_t* inner_ptrs0[0x10] = { (void*)g_Data,    (void*)g_Data,     (void*)g_Data,
                                          (void*)g_Data,    (void*)0x41414141, (void*)0x42424242,
                                          (void*)0x43434343 };
    const uint32_t* inner_ptrs1[0x10] = { (void*)g_Data, (void*)g_Data, (void*)g_Data,
                                          (void*)g_Data, (void*)g_Data, (void*)g_Data,
                                          (void*)g_Data };
    const uint32_t* inner_ptrs2[0x10] = { (void*)0x43434343, (void*)g_Data, (void*)g_Data,
                                          (void*)g_Data,     (void*)g_Data, (void*)g_Data,
                                          (void*)g_Data };
    const uint32_t** ptrs[] = { inner_ptrs0, inner_ptrs1, inner_ptrs2 };
    const uint32_t** p = ptrs[3 + symbolic_index_outer];
    const uint32_t* final_ptr = p[2 + symbolic_index_inner];
    [[maybe_unused]] const volatile uint32_t v =
        *(volatile uint32_t*)final_ptr; // this one should be marked as symbolic,
}

ENTRY_POINT_ATTR void start()
{
    unsafe_symbolic_table_multiple_levels_signed();
    ENTRY_POINT_END();
}
