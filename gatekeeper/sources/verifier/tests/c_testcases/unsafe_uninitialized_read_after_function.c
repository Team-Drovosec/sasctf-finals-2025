#include "trusted_lib_api.h"

__attribute__((noinline)) void unsafe_uninitialized_read_simple()
{
    uint8_t data[128];
    [[maybe_unused]] uint64_t uninit = *(volatile uint64_t*)&data[0];
}

__attribute__((noinline)) void unsafe_uninitialized_read_after_function()
{
    uint8_t data[16];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);
    // uint64_t uninit = *(volatile uint64_t*)(&data[0] - 0xc);
    unsafe_uninitialized_read_simple();
}

ENTRY_POINT_ATTR void start()
{
    unsafe_uninitialized_read_after_function();
    ENTRY_POINT_END();
}
