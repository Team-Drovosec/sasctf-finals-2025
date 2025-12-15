#include "trusted_lib_api.h"

__attribute__((noinline)) void unsafe_read_file_plain_symbolic_controlflow_through_ret()
{
    uint8_t data[16];
    uint32_t len = 16;

    int res = TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);

    if (res == -1) {
        TRUSTED_CALL_VOID(mini_printf, "An error occurred trying to read the file!\n");
    }
}
ENTRY_POINT_ATTR void start()
{
    unsafe_read_file_plain_symbolic_controlflow_through_ret();
    ENTRY_POINT_END();
}
