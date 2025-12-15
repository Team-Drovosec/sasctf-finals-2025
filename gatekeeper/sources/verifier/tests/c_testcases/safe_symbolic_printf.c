#include "trusted_lib_api.h"

__attribute__((noinline)) void safe_symbolic_printf()
{
    uint8_t data[16];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);

    uint32_t symbolic_counter = *(uint32_t*)&data[0];

    TRUSTED_CALL_VOID(mini_printf, "%s, %d\n", data, symbolic_counter);
}

ENTRY_POINT_ATTR void start()
{
    safe_symbolic_printf();
    ENTRY_POINT_END();
}
