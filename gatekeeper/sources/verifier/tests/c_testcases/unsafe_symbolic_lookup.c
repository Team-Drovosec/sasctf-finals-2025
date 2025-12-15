#include "trusted_lib_api.h"

void do_exit()
{
    ENTRY_POINT_END();
}

__attribute__((noinline)) void unsafe_symbolic_lookup()
{
    uint8_t data[16];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);

    uint32_t symbolic_var = *(volatile uint32_t*)&data[0];

    const void* ptrs[] = { &do_exit,          (void*)0x41414141, (void*)0x42424242,
                           (void*)0x43434343, (void*)0x44444444, (void*)0x45454545,
                           (void*)0x46464646, (void*)0x47474747, (void*)0x48484848,
                           (void*)0x49494949, (void*)0x4A4A4A4A, (void*)0x4B4B4B4B,
                           (void*)0x4C4C4C4C, (void*)0x4D4D4D4D, (void*)0x4E4E4E4E,
                           (void*)0x4F4F4F4F, (void*)0x50505050, (void*)0x51515151,
                           (void*)0x52525252, (void*)0x53535353 };
    ((void (*)(void))ptrs[symbolic_var])(); // Symbolic function pointer call!
}

ENTRY_POINT_ATTR void start()
{
    unsafe_symbolic_lookup();
    ENTRY_POINT_END();
}
