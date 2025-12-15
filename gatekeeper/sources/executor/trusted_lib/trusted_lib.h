#ifndef __TRUSTED_LIB_H__
#define __TRUSTED_LIB_H__

#include <stdint.h>

#define HANDLES_BASE 0x13400000
#define MMIO_BASE 0x13370000
#define HANDLES_MAX_ENTRIES ((0x80000 - sizeof(uint32_t)) / sizeof(HandleEntry_t))
#define UART0_BASE 0x10000000

#define MAX_FILE_SIZE 2048 + 12 // 2 KiB + IV

typedef struct __attribute__((packed))
{
    uint64_t public_handle;
    uint64_t private_handle;
    uint32_t encrypted;
} HandleEntry_t;

typedef struct
{
    uint32_t total_entries;
    HandleEntry_t entries[HANDLES_MAX_ENTRIES];
} HandleTable_t;

#define MODE_CREATE_PLAIN 0x1100
#define MODE_CREATE_ENCRYPT 0x1101
#define MODE_READ_PLAIN 0x1200
#define MODE_READ_ENCRYPT 0x1201

#define REG_MODE 0x00
#define REG_STATUS 0x04
#define REG_LENDW 0x08
#define REG_FILEDW 0x0C
#define REG_PUB_HANDLE_LO 0x10
#define REG_PUB_HANDLE_HI 0x14
#define REG_PRIV_HANDLE_LO 0x18
#define REG_PRIV_HANDLE_HI 0x1C
#define REG_IV0 0x20
#define REG_IV1 0x24
#define REG_IV2 0x28
#define REG_TAG0 0x2C
#define REG_TAG1 0x30
#define REG_TAG2 0x34
#define REG_TAG3 0x38

#define GATEKEEPER_EOK 0
#define GATEKEEPER_ENOSPACE 1
#define GATEKEEPER_ECREAT 2
#define GATEKEEPER_ENOFILE 3
#define GATEKEEPER_EBUSY 4
#define GATEKEEPER_ECORRUPTED 5

#define TRUSTED_API(NAME) __attribute__((section(#NAME), used, noinline))

TRUSTED_API(.CREATE_FILE_PLAIN)
uint64_t create_file_plain(uint8_t* fbytes, uint32_t len);

TRUSTED_API(.CREATE_FILE_ENC)
uint64_t create_file_enc(uint8_t* fbytes, uint32_t len, uint32_t* iv, uint32_t* tag);

TRUSTED_API(.READ_FILE_PLAIN)
int32_t read_file_plain(uint64_t public_handle, uint8_t* fbytes, uint32_t* len);

TRUSTED_API(.READ_FILE_ENC)
int32_t read_file_enc(uint64_t public_handle, uint32_t* tag, uint8_t* fbytes, uint32_t* len);

TRUSTED_API(.MINI_PRINTF) void mini_printf(const char* fmt, ...);

extern void __trusted_scrub_after_epilogue_keep_a0();

__attribute__((noinline)) static void __trusted_make_nonleaf(void)
{
    __asm__ volatile("" ::: "memory");
}

#define TRUSTED_MAYBE_FORCE_NONLEAF()                                                              \
    do {                                                                                           \
        __trusted_make_nonleaf();                                                                  \
    } while (0)

#define __TRUSTED_RETURN_COMMON(val)                                                               \
    /* Place return value */                                                                       \
    register uintptr_t __rv = (uintptr_t)(val);                                                    \
    __asm__ volatile("mv a0, %0" ::"r"(__rv) : "a0", "memory");                                    \
                                                                                                   \
    /* Locate saved RA slot and original RA */                                                     \
    void* fp = __builtin_frame_address(0); /* requires -fno-omit-frame-pointer */                  \
    void** __slot = (void**)((char*)fp - (int)sizeof(void*));                                      \
    void* __orig_ra = *__slot;                                                                     \
                                                                                                   \
    /* Patch saved RA to trampoline; pass original RA in a1 */                                     \
    *__slot = (void*)&__trusted_scrub_after_epilogue_keep_a0;                                      \
    __asm__ volatile("mv a1, %0" ::"r"(__orig_ra) : "a1", "memory");

#define TRUSTED_RETURN(val)                                                                        \
    do {                                                                                           \
        __TRUSTED_RETURN_COMMON(val);                                                              \
        /* Normal C return; epilogue restores s* sp, loads RA = trampoline, ret */                 \
        return (typeof(val))__rv;                                                                  \
    } while (0)

#define TRUSTED_RETURN_VOID()                                                                      \
    do {                                                                                           \
        __TRUSTED_RETURN_COMMON(0);                                                                \
        /* Normal C return; epilogue restores s* sp, loads RA = trampoline, ret */                 \
        return;                                                                                    \
    } while (0)

#endif // __TRUSTED_LIB_H__
