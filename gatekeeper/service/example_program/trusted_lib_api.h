#ifndef __ABI_H__
#define __ABI_H__

// Trusted Library API
// This file defines the interface between the application and the QEMU
// environment. It includes function pointers for file operations and
// macros for defining the entry point of the application.

#include <stdint.h>

/**
 * Given a value `val`, branchlessly produce a new value that is guaranteed to lie within the range
 * [min, max]. This is used to guide the verifier about possible value ranges of untrusted input, so
 * that they can be safely used as array indices or similar.
 */
uint32_t verifier_ranged_u32(uint32_t v, uint32_t lo, uint32_t hi);

/**
 * Halt execution of the program.
 */
__attribute__((noreturn, noinline)) void halt(void);

#define ENTRY_POINT_ATTR __attribute__((section(".entry"), noreturn))

#define ENTRY_POINT_END() halt();

#if defined(__clang__) || defined(__GNUC__)
#define TRUSTED_NOINLINE __attribute__((noinline, noclone))
#else
#define TRUSTED_NOINLINE
#endif

#if defined(__riscv) && __riscv_xlen == 64

/* Clobber: all caller-saved registers in RV64
 * Reason: Trusted APIs zero-out all registers it does not restore, so that no information is leaked
 * between calls.
 */
#define __TRUSTED_RV64_CLOBBERS                                                                    \
    "memory", "x5", "x6", "x7", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x28", "x29",     \
        "x30", "x31"

#define TRUSTED_CALL(fn, ...)                                                                      \
    (__extension__({                                                                               \
        typeof(fn(__VA_ARGS__)) __ret = fn(__VA_ARGS__);                                           \
        __asm__ volatile("" ::: __TRUSTED_RV64_CLOBBERS);                                          \
        __ret;                                                                                     \
    }))

#define TRUSTED_CALL_VOID(fn, ...)                                                                 \
    do {                                                                                           \
        fn(__VA_ARGS__);                                                                           \
        __asm__ volatile("" ::: __TRUSTED_RV64_CLOBBERS);                                          \
    } while (0)

#else
#error "trusted_lib_api.h supports only RV64."
#endif

typedef uint64_t (*create_file_plain_t)(uint8_t* fbytes, uint32_t len);
typedef uint64_t (*create_file_enc_t)(uint8_t* fbytes, uint32_t len, uint32_t* iv, uint32_t* tag);
typedef int32_t (*read_file_plain_t)(uint64_t public_handle, uint8_t* fbytes, uint32_t* fsize);
typedef int32_t (*read_file_enc_t)(uint64_t public_handle,
                                   uint32_t* auth_tag,
                                   uint8_t* fbytes,
                                   uint32_t* len);
typedef void (*mini_printf_t)(const char* fmt, ...);

static const create_file_plain_t create_file_plain = (create_file_plain_t)0x80000000;
static const create_file_enc_t create_file_enc = (create_file_enc_t)0x80002000;
static const read_file_plain_t read_file_plain = (read_file_plain_t)0x80004000;
static const read_file_enc_t read_file_enc = (read_file_enc_t)0x80006000;
static const mini_printf_t mini_printf = (mini_printf_t)0x8000A000;

#endif // __ABI_H__