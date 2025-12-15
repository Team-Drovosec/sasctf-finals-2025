#include "trusted_lib_api.h"

__attribute__((noinline)) void safe_symbolic_ranged_u32_stress_test()
{
    uint8_t buffer[128] = { 0 };
    uint8_t data[128];
    uint32_t len = sizeof(data);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, data, &len);

    for (uint32_t i = 0; i < sizeof(data) / 4; i++) {
        uint32_t sym_val = *(volatile uint32_t*)&data[i * 4];
        // this shit is muuuuch slower due to ITE-modelling.
        // uint32_t ranged_index = TRUSTED_CALL(verifier_ranged_u32, sym_val, 0, sizeof(buffer) -
        // 1);
        uint32_t ranged_index = verifier_ranged_u32(sym_val, 0, sizeof(buffer) - 1);
        [[maybe_unused]] uint8_t val = *(volatile uint8_t*)&buffer[ranged_index];
    }
}

ENTRY_POINT_ATTR void start()
{
    safe_symbolic_ranged_u32_stress_test();
    ENTRY_POINT_END();
}
