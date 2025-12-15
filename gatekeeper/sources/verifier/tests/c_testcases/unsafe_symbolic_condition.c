#include "trusted_lib_api.h"

void do_exit()
{
    ENTRY_POINT_END();
}

__attribute__((noinline)) void unsafe_symbolic_condition()
{
    uint8_t data[16];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);

    uint32_t symbolic_var = *(uint32_t*)&data[0];

    if (symbolic_var == 0x13371337) {
        *(volatile uint32_t*)(0x41414141) = 0x42424242;
    } else {
        do_exit();
    }
}

ENTRY_POINT_ATTR void start()
{
    unsafe_symbolic_condition();
    ENTRY_POINT_END();
}
