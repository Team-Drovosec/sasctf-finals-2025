#include "trusted_lib_api.h"

__attribute__((noinline)) void unsafe_symptr_might_point_to_uninitialized_data()
{
    uint64_t data[16];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, (uint8_t*)data, &len);

    uint32_t symbolic_var = verifier_ranged_u32(*(volatile uint32_t*)&data[0], 0, 255);
    int32_t signed_sym_var = -(int32_t)symbolic_var;
    [[maybe_unused]] uint64_t uninit = *(volatile uint64_t*)(&data[signed_sym_var]);
}

ENTRY_POINT_ATTR void start()
{
    unsafe_symptr_might_point_to_uninitialized_data();
    ENTRY_POINT_END();
}
