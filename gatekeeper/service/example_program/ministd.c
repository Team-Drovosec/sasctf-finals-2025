// ministd.c
#include "ministd.h"
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>

/* ====================== memory ====================== */

// memset
void* memset(void* s, int c, size_t n)
{
    uint8_t* p = (uint8_t*)s;
    while (n--) {
        *p++ = (uint8_t)c;
    }
    return s;
}

// memcpy
void* memcpy(void* restrict dst, const void* restrict src, size_t n)
{
    uint8_t* d = (uint8_t*)dst;
    const uint8_t* s = (const uint8_t*)src;
    while (n--)
        *d++ = *s++;
    return dst;
}

// memcmp
int memcmp(const void* a, const void* b, size_t n)
{
    const uint8_t* p = (const uint8_t*)a;
    const uint8_t* q = (const uint8_t*)b;
    for (; n; --n, ++p, ++q) {
        if (*p != *q)
            return (int)*p - (int)*q;
    }
    return 0;
}

// memchr
void* memchr(const void* s, int c, size_t n)
{
    const uint8_t* p = (const uint8_t*)s;
    uint8_t target = (uint8_t)c;
    for (; n; --n, ++p) {
        if (*p == target)
            return (void*)p;
    }
    return NULL;
}

// memxor (non-standard; XOR src into dst)
void* memxor(void* dst, const void* src, size_t n)
{
    uint8_t* d = (uint8_t*)dst;
    const uint8_t* s = (const uint8_t*)src;
    while (n--)
        *d++ ^= *s++;
    return dst;
}

/* ====================== strings ====================== */

// strlen
size_t strlen(const char* s)
{
    const char* p = s;
    while (*p)
        ++p;
    return (size_t)(p - s);
}

// strnlen
size_t strnlen(const char* s, size_t maxlen)
{
    size_t i = 0;
    while (i < maxlen && s[i])
        ++i;
    return i;
}

// strcmp
int strcmp(const char* a, const char* b)
{
    while (*a && (*a == *b)) {
        ++a;
        ++b;
    }
    return (int)(uint8_t)*a - (int)(uint8_t)*b;
}

// strncmp
int strncmp(const char* a, const char* b, size_t n)
{
    for (size_t i = 0; i < n; ++i) {
        uint8_t ac = (uint8_t)a[i];
        uint8_t bc = (uint8_t)b[i];
        if (ac != bc)
            return (int32_t)ac - (int32_t)bc;
        if (ac == 0)
            return 0;
    }
    return 0;
}

// strcpy
char* strcpy(char* dst, const char* src)
{
    char* r = dst;
    while ((*dst++ = *src++)) { /* empty */
    }
    return r;
}

// strncpy (pads with zeros if src shorter than n)
char* strncpy(char* dst, const char* src, size_t n)
{
    size_t i = 0;
    for (; i < n && src[i]; ++i)
        dst[i] = src[i];
    for (; i < n; ++i)
        dst[i] = '\0';
    return dst;
}

/* ====================== hex utils ====================== */
/* Buffer sizing rules:
   - hex_encode(): dst must have at least 2*n + 1 bytes (null-terminated).
   - hex_decode(): returns number of bytes written, or negative on error.
*/

static inline int hex_nibble(int c)
{
    uint32_t x = (uint32_t)(uint8_t)c;

    /* decimal: '0'..'9' */
    uint32_t dec = x - (uint32_t)'0';
    uint32_t mdec = 0u - (dec <= 9u); /* 0xFFFFFFFF if in range, else 0 */

    /* alpha: fold to lowercase and check 'a'..'f' */
    uint32_t lo = (x | 0x20u) - (uint32_t)'a'; /* 'A'/'a' -> 0, ... */
    uint32_t malp = 0u - (lo <= 5u);           /* 0xFFFFFFFF if in range, else 0 */

    /* value if valid (masked), otherwise 0 */
    uint32_t val = (dec & mdec) | ((lo + 10u) & malp);

    /* if neither dec nor alpha matched, return -1; else return val */
    uint32_t many = mdec | malp;        /* 0xFFFFFFFF if any matched */
    return (int)((val & many) | ~many); /* ~0 => -1 */
}

// hex_encode: bytes -> hex string (null-terminated). Returns written chars
// (2*n).
size_t hex_encode(const void* src, size_t n, char* dst, int uppercase)
{
    const unsigned char* s = (const unsigned char*)src;
    const char* digits = uppercase ? "0123456789ABCDEF" : "0123456789abcdef";
    for (size_t i = 0; i < n; ++i) {
        unsigned char b = s[i];
        dst[2 * i] = digits[(b >> 4) & 0xF];
        dst[2 * i + 1] = digits[b & 0xF];
    }
    dst[2 * n] = '\0';
    return 2 * n;
}

// hex_decode: valid hex string (only contains A-F, 0-9) -> bytes.
// Returns bytes written, or -1 bad length, -2 overflow.
int hex_decode(const char* hex, size_t hex_len, void* dst, size_t dst_cap)
{
    if (hex_len % 2 != 0)
        return -1; // bad length

    size_t out_n = hex_len / 2;
    if (out_n > dst_cap)
        return -2; // not enough space

    uint8_t* d = (uint8_t*)dst;
    for (size_t i = 0; i < out_n; ++i) {
        int32_t hi = hex_nibble(hex[2 * i]);
        int32_t lo = hex_nibble(hex[2 * i + 1]);
        d[i] = (uint8_t)((hi << 4) | lo);
    }
    return (int32_t)out_n;
}

// u64_to_hex: fixed-width hex (no prefix). width=1..16. Returns buf.
char* u64_to_hex(uint64_t v, char* buf, unsigned width, int uppercase)
{
    if (width == 0)
        width = 1;
    if (width > 16)
        width = 16;
    const char* digits = uppercase ? "0123456789ABCDEF" : "0123456789abcdef";

    buf[width] = '\0';
    for (uint32_t i = 0; i < width; ++i) {
        buf[width - 1 - i] = digits[(uint32_t)(v & 0xF)];
        v >>= 4;
    }
    return buf;
}
