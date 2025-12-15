#include "trusted_lib_api.h"

__attribute__((noinline)) void unsafe_symbolic_controlflow()
{
    uint8_t data[16];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);

    register uint32_t symbolic_counter asm("x7") = *(uint32_t*)&data[0];
    register uint32_t i asm("x9") = 0;
    register uint32_t flag asm("x5") = 0;

    for (; i < symbolic_counter; i++) { // Get all possible values of `symbolic_counter`?
        if (*(volatile uint32_t*)&data[4] == 0xdeadbeef) {
            break;
        }

        if (i == 0x13371337) {
            flag = 1;
        }
    }

    if (flag) {
        // To actually catch this testcase, we somehow need to check
        // __all__ values for `symbolic_counter`.
        // So this testcase should be prohibited somehow?
        *(volatile uint32_t*)(0x41414141) = 0x42424242;
    }
}

ENTRY_POINT_ATTR void start()
{
    unsafe_symbolic_controlflow();
    ENTRY_POINT_END();
}
