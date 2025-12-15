#include "trusted_lib_api.h"
#include <stdint.h>

/**
 * Halt execution of the program.
 */
__attribute__((noreturn, noinline)) void halt(void)
{
    __asm__ __volatile__("ebreak\n\tj .");
    __builtin_unreachable();
}

/**
 * Given a value `val`, branchlessly produce a new value that is guaranteed to lie within the range
 * [min, max]. This is used to guide the verifier about possible value ranges of untrusted input, so
 * that they can be safely used as array indices or similar.
 */
__attribute__((optimize("O0"), optnone, noipa, noinline)) uint32_t verifier_ranged_u32(uint32_t v,
                                                                                       uint32_t lo,
                                                                                       uint32_t hi)
{
    v ^= (v ^ lo) & (uint32_t) - (v < lo);
    v ^= (v ^ hi) & (uint32_t) - (v > hi);
    return v;
}