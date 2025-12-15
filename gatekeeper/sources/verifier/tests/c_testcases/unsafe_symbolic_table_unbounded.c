#include "trusted_lib_api.h"

static uint8_t g_Data[128] = { 0 };

__attribute__((noinline)) void unsafe_symbolic_table_unbounded()
{
    uint8_t data[16];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);

    uint32_t symbolic_var = *(volatile uint32_t*)&data[0];
    const uint32_t* ptrs[] = { (void*)g_Data,     (void*)g_Data,     (void*)g_Data,
                               (void*)0x41414141, (void*)0x42424242, (void*)0x43434343 };
    const uint32_t* p = ptrs[symbolic_var];
    [[maybe_unused]] const volatile uint32_t v =
        *(volatile uint32_t*)p; // this one should be marked as symbolic, and
                                // unsafe, since symbolic_var is unbounded.
}

ENTRY_POINT_ATTR void start()
{
    unsafe_symbolic_table_unbounded();
    ENTRY_POINT_END();
}
