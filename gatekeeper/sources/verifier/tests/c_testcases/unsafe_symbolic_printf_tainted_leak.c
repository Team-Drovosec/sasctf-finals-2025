#include "trusted_lib_api.h"

__attribute__((noinline)) void unsafe_symbolic_printf_tainted_leak()
{
    uint8_t uninit[16];
    uint8_t data[16];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);

    uint32_t symbolic_counter = *(volatile uint32_t*)&data[0];

    TRUSTED_CALL_VOID(mini_printf, "%s, %d\n", (char*)&uninit[0], symbolic_counter);
}

ENTRY_POINT_ATTR void start()
{
    unsafe_symbolic_printf_tainted_leak();
    ENTRY_POINT_END();
}
