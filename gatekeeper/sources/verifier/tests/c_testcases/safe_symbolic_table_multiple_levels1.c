#include "trusted_lib_api.h"

static uint8_t g_Data[128] = { 0 };

__attribute__((noinline)) void safe_symbolic_table_multiple_levels1()
{
    uint8_t data[32];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);

    const uint32_t* inner_ptrs0[0x60] = { (void*)g_Data,    (void*)g_Data,     (void*)g_Data,
                                          (void*)g_Data,    (void*)0x41414141, (void*)0x42424242,
                                          (void*)0x43434343 };
    const uint32_t* inner_ptrs1[0x60] = { (void*)g_Data, (void*)g_Data, (void*)g_Data,
                                          (void*)g_Data, (void*)g_Data, (void*)g_Data,
                                          (void*)g_Data };
    const uint32_t* inner_ptrs2[0x60] = { (void*)g_Data, (void*)g_Data, (void*)g_Data,
                                          (void*)g_Data, (void*)g_Data, (void*)g_Data,
                                          (void*)g_Data };

    const uint32_t** ptrs[] = { inner_ptrs0, inner_ptrs1, inner_ptrs2 };

    uint32_t symbolic_index_outer0 = verifier_ranged_u32(*(volatile uint32_t*)&data[0], 1, 2);
    uint32_t symbolic_index_outer1 = verifier_ranged_u32(*(volatile uint32_t*)&data[4], 0, 0);

    const uint32_t** p = ptrs[symbolic_index_outer0];
    const uint32_t* final_ptr = p[symbolic_index_outer1];
    [[maybe_unused]] const volatile uint32_t v =
        *(volatile uint32_t*)final_ptr; // this one should be marked as symbolic,
                                        // and safe!
}

ENTRY_POINT_ATTR void start()
{
    safe_symbolic_table_multiple_levels1();
    ENTRY_POINT_END();
}
