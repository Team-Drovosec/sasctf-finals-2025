#include "trusted_lib_api.h"

static uint8_t g_Data[128] = { 0 };

__attribute__((noinline)) void unsafe_simple_symbolic_ptr()
{
    uint64_t data[16];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, (uint8_t*)data, &len);

    uint8_t* constant_ptr_safe = g_Data;
    uint64_t symbolic_offset_unsafe = (*(uint64_t*)&data[0]);
    [[maybe_unused]] uint64_t unbounded =
        *(volatile uint64_t*)(constant_ptr_safe + symbolic_offset_unsafe);
}

ENTRY_POINT_ATTR void start()
{
    unsafe_simple_symbolic_ptr();
    ENTRY_POINT_END();
}
