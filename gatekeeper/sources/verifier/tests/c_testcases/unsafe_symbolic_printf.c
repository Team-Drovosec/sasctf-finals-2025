#include "trusted_lib_api.h"

__attribute__((noinline)) void unsafe_symbolic_printf()
{
    uint8_t data[16];
    uint32_t len = sizeof(data);

    const char* symbolic_str = "Hello from symbolic string!";

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);

    uint32_t symbolic_counter = *(volatile uint32_t*)&data[0];
    volatile char* symbolic_pointer = symbolic_str + *(volatile uint32_t*)&data[4];

    TRUSTED_CALL_VOID(mini_printf, "%s, %d\n", symbolic_pointer, symbolic_counter);
}

ENTRY_POINT_ATTR void start()
{
    unsafe_symbolic_printf();
    ENTRY_POINT_END();
}
