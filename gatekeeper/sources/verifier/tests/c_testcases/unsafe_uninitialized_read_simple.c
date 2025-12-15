#include "trusted_lib_api.h"

__attribute__((noinline)) void unsafe_uninitialized_read_simple()
{
    uint8_t data[128];
    [[maybe_unused]] uint64_t uninit = *(volatile uint64_t*)&data[0];
}

ENTRY_POINT_ATTR void start()
{
    unsafe_uninitialized_read_simple();
    ENTRY_POINT_END();
}
