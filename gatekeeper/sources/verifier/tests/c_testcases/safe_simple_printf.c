#include "ministd.h"
#include "trusted_lib_api.h"

__attribute__((noinline)) void safe_simple_printf()
{
    char str[] = "test!";
    // char hex_encoded[sizeof(str) * 2 + 1];
    // hex_encode(str, strlen(str), hex_encoded, 0);

    uint32_t val0 = verifier_ranged_u32(0xdeadbeef, 0, 0xffffffff);
    uint32_t val1 = verifier_ranged_u32(0x0, 0x41414141, 0xffffffff);
    uint32_t val2 = verifier_ranged_u32(0x43434343, 0, 0x42424242);

    uint64_t rand = COMPTIME_RAND_U64();
    TRUSTED_CALL_VOID(
        mini_printf,
        "Hello, World!! %%, 0x%llx, 0x%x, %d, %c, %s, %p, 0x%x, 0x%x, 0x%x, 0x%x, "
        "0x%x, 0x%x, 0x%x, 0x%x, 0x%x, 0x%x, 0x%llx, val0=0x%x, val1=0x%x, val2=0x%x\n",
        0x123456789abcdef0ULL,
        0xdeadbeef,
        -12345,
        'A',
        str,
        str,
        0x13371337,
        0x41414141,
        0x42424242,
        0x43434343,
        0x44444444,
        0x45454545,
        0x46464646,
        0x47474747,
        0x48484848,
        0x49494949,
        rand,
        val0,
        val1,
        val2);

    TRUSTED_CALL_VOID(mini_printf, "TEST: 0x%llx, 0x%x\n", 0x123456789abcdef0ULL, 0xdeadbeef);
}

ENTRY_POINT_ATTR void start()
{
    safe_simple_printf();
    ENTRY_POINT_END();
}
