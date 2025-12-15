#ifndef __MINISTD_H__
#define __MINISTD_H__

#include <stddef.h>
#include <stdint.h>

/* Fail at compile time if max < min, without evaluating anything. */
#define __CRAND_CHECK(min, max)                                                                    \
    _Static_assert((unsigned long long)(max) >= (unsigned long long)(min),                         \
                   "COMPTIME_RAND: max < min")

#ifndef COMPTIME_SEED
#define COMPTIME_SEED 0ULL
#endif

/* 64-bit seed from counter + line number + time */
#define __CRAND_SEED64                                                                             \
    (((unsigned long long)(__COUNTER__ + 1u)) | ((unsigned long long)(__LINE__) << 32)) ^          \
        (unsigned long long)(COMPTIME_SEED)

/* 64-bit LCG */
#define __CRAND_RAW64 (0x5851f42d4c957f2dULL * (__CRAND_SEED64) + 1ULL)

/* Fallback: uses top 32 bits of the 64-bit product; small bias if range > 2^32. */
#if defined(__SIZEOF_INT128__)
#define __CRAND_SCALE(raw, range)                                                                  \
    ((unsigned long long)(((__uint128_t)(raw) * (__uint128_t)(range)) >> 64))
#else
#error "COMPTIME_RAND requires __SIZEOF_INT128__ to be defined"
#endif

/* Public API: COMPTIME_RAND(min, max) -> integer in [min, max] as a constant expression */
#define COMPTIME_RAND(min, max)                                                                    \
    (__CRAND_SCALE(__CRAND_RAW64, ((unsigned long long)(max) - (unsigned long long)(min) + 1ULL)))

#define COMPTIME_RAND_U8() COMPTIME_RAND(0u, 0xfeu)
#define COMPTIME_RAND_U32() COMPTIME_RAND(0u, 0xfffffffeu)
#define COMPTIME_RAND_U64() COMPTIME_RAND(0ULL, 0xfffffffffffffffeULL)

/* ----------------------------------------------------------------------- */

void* memset(void* s, int c, size_t n);
void* memcpy(void* dst, const void* src, size_t n);
void* memmove(void* dst, const void* src, size_t n);
int memcmp(const void* a, const void* b, size_t n);
void* memchr(const void* s, int c, size_t n);
void bzero(void* s, size_t n);
void* memxor(void* dst, const void* src, size_t n);

size_t strlen(const char* s);
size_t strnlen(const char* s, size_t maxlen);
int strcmp(const char* a, const char* b);
int strncmp(const char* a, const char* b, size_t n);
char* strcpy(char* dst, const char* src);
char* strncpy(char* dst, const char* src, size_t n);

size_t hex_encode(const void* src, size_t n, char* dst, int uppercase);
int hex_decode(const char* hex, size_t hex_len, void* dst, size_t dst_cap);
char* u64_to_hex(uint64_t v, char* buf, unsigned width, int uppercase);

#endif // __MINISTD_H__
