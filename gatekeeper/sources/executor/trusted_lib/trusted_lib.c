// TODO: truncate here on the client side
// TODO: define invalid handle value

#include "trusted_lib.h"

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>

volatile HandleTable_t* g_HandleTable = (volatile HandleTable_t*)HANDLES_BASE;

union u64_t
{
    uint32_t u32[2];
    uint64_t u64;
};

void* memset(void* s, int32_t c, size_t n)
{
    uint8_t* p = (uint8_t*)s;
    while (n--) {
        *p++ = (uint8_t)c;
    }
    return s;
}

uint64_t get_private_handle(uint64_t public_handle, uint64_t* private_handle)
{
    if (private_handle == NULL) {
        return (uint64_t)-1;
    }

    uint64_t found = 0;
    uint64_t found_private = 0;

    for (uint32_t i = 0; i < HANDLES_MAX_ENTRIES; ++i) {
        uint64_t ph = g_HandleTable->entries[i].public_handle;
        uint64_t x = ph ^ public_handle;
        uint64_t mask = 0 - (uint64_t)(x == 0ULL);
        found_private |= (g_HandleTable->entries[i].private_handle & mask);
        found |= (g_HandleTable->entries[i].private_handle != 0ULL) ? mask : 0ULL;
    }

    if (found) {
        *private_handle = found_private;
        return 0;
    } else {
        return (uint64_t)-1;
    }
}

static inline void _mmio_write_dw(uint64_t base, uint32_t offset, uint32_t val)
{
    *(volatile uint32_t*)(base + offset) = val;
}

static inline uint32_t _mmio_read_dw(uint64_t base, uint32_t offset)
{
    return *(volatile uint32_t*)(base + offset);
}

static inline uint64_t _mmio_read_qw_lohi(uint64_t base, uint32_t offset_lo, uint32_t offset_hi)
{
    union u64_t result;
    result.u32[0] = _mmio_read_dw(base, offset_lo);
    result.u32[1] = _mmio_read_dw(base, offset_hi);

    return result.u64;
}

static inline void _mmio_write_qw_lohi(uint64_t base,
                                       uint32_t offset_lo,
                                       uint32_t offset_hi,
                                       uint64_t val)
{
    // uint32_t lo = (val & 0xFFFFFFFF);
    // uint32_t hi = (val >> 32) & 0xFFFFFFFF;

    union u64_t res;
    res.u64 = val;

    _mmio_write_dw(base, offset_lo, res.u32[0]);
    _mmio_write_dw(base, offset_hi, res.u32[1]);
}

static inline void _mmio_write_b(uint64_t base, uint32_t offset, uint8_t val)
{
    *(volatile uint8_t*)(base + offset) = val;
}

uint64_t _mmio_create_plain(uint8_t* fbytes,
                            uint32_t len,
                            uint64_t* private_handle,
                            uint64_t* public_handle)
{
    _mmio_write_dw(MMIO_BASE, REG_MODE, MODE_CREATE_PLAIN);
    uint32_t mode_status = _mmio_read_dw(MMIO_BASE, REG_STATUS);
    if (mode_status == GATEKEEPER_ENOSPACE) {
        return (uint64_t)-1;
    }

    _mmio_write_dw(MMIO_BASE, REG_LENDW, len / 4);

    for (uint32_t i = 0; i < len / 4; ++i) {
        _mmio_write_dw(MMIO_BASE, REG_FILEDW, ((uint32_t*)fbytes)[i]);
    }

    *private_handle = _mmio_read_qw_lohi(MMIO_BASE, REG_PRIV_HANDLE_LO, REG_PRIV_HANDLE_HI);
    *public_handle = _mmio_read_qw_lohi(MMIO_BASE, REG_PUB_HANDLE_LO, REG_PUB_HANDLE_HI);

    return _mmio_read_dw(MMIO_BASE, REG_STATUS);
}

TRUSTED_API(.CREATE_FILE_PLAIN)
uint64_t create_file_plain(uint8_t* fbytes, uint32_t len)
{
    TRUSTED_MAYBE_FORCE_NONLEAF();

    if (fbytes == NULL || len % 4 != 0 || len == 0) {
        TRUSTED_RETURN(-1);
    }
    uint64_t public_handle = 0;
    uint64_t private_handle = 0;

    if (len > MAX_FILE_SIZE) {
        len = MAX_FILE_SIZE;
    }

    uint64_t mmio_status = _mmio_create_plain(fbytes, len, &private_handle, &public_handle);
    if (mmio_status == GATEKEEPER_EOK) {
        TRUSTED_RETURN(public_handle);
    }
    TRUSTED_RETURN(0);
}

uint64_t _mmio_create_enc(uint8_t* fbytes,
                          uint32_t len,
                          uint32_t* iv,
                          uint32_t* tag,
                          uint64_t* private_handle,
                          uint64_t* public_handle)
{
    _mmio_write_dw(MMIO_BASE, REG_MODE, MODE_CREATE_ENCRYPT);
    uint32_t mode_status = _mmio_read_dw(MMIO_BASE, REG_STATUS);
    if (mode_status == GATEKEEPER_ENOSPACE) {
        return (uint64_t)-1;
    }

    _mmio_write_dw(MMIO_BASE, REG_IV0, iv[0]);
    _mmio_write_dw(MMIO_BASE, REG_IV1, iv[1]);
    _mmio_write_dw(MMIO_BASE, REG_IV2, iv[2]);

    _mmio_write_dw(MMIO_BASE, REG_LENDW, len / 4);

    for (uint32_t i = 0; i < len / 4; ++i) {
        _mmio_write_dw(MMIO_BASE, REG_FILEDW, ((uint32_t*)fbytes)[i]);
    }

    tag[0] = _mmio_read_dw(MMIO_BASE, REG_TAG0);
    tag[1] = _mmio_read_dw(MMIO_BASE, REG_TAG1);
    tag[2] = _mmio_read_dw(MMIO_BASE, REG_TAG2);
    tag[3] = _mmio_read_dw(MMIO_BASE, REG_TAG3);

    *private_handle = _mmio_read_qw_lohi(MMIO_BASE, REG_PRIV_HANDLE_LO, REG_PRIV_HANDLE_HI);
    *public_handle = _mmio_read_qw_lohi(MMIO_BASE, REG_PUB_HANDLE_LO, REG_PUB_HANDLE_HI);

    return _mmio_read_dw(MMIO_BASE, REG_STATUS);
}

TRUSTED_API(.CREATE_FILE_ENC)
uint64_t create_file_enc(uint8_t* fbytes, uint32_t len, uint32_t* iv, uint32_t* tag)
{
    TRUSTED_MAYBE_FORCE_NONLEAF();

    if (fbytes == NULL || iv == NULL || tag == NULL || len % 4 != 0 || len == 0) {
        TRUSTED_RETURN(-1);
    }

    uint64_t public_handle = 0;
    uint64_t private_handle = 0;

    if (len > MAX_FILE_SIZE - 12) {
        len = MAX_FILE_SIZE - 12; // deduce the size of iv
    }

    uint64_t mmio_status = _mmio_create_enc(fbytes, len, iv, tag, &private_handle, &public_handle);
    if (mmio_status == GATEKEEPER_EOK) {
        TRUSTED_RETURN(public_handle);
    }

    TRUSTED_RETURN(0);
}

uint64_t _mmio_read_plain(uint8_t* fbytes, uint32_t* len, uint64_t private_handle)
{
    _mmio_write_qw_lohi(MMIO_BASE, REG_PRIV_HANDLE_LO, REG_PRIV_HANDLE_HI, private_handle);
    _mmio_write_dw(MMIO_BASE, REG_MODE, MODE_READ_PLAIN);

    uint32_t tmp_len = _mmio_read_dw(MMIO_BASE, REG_LENDW);
    tmp_len *= 4;

    if (tmp_len > *len) {
        return (uint64_t)-1;
    }

    *len = tmp_len;

    for (uint32_t i = 0; i < *len / 4; ++i) {
        uint32_t* dw_ptr = (uint32_t*)fbytes + i;
        *dw_ptr = _mmio_read_dw(MMIO_BASE, REG_FILEDW);
    }

    return _mmio_read_dw(MMIO_BASE, REG_STATUS);
}

TRUSTED_API(.READ_FILE_PLAIN)
int32_t read_file_plain(uint64_t public_handle, uint8_t* fbytes, uint32_t* len)
{
    TRUSTED_MAYBE_FORCE_NONLEAF();

    if (fbytes == NULL || len == NULL) {
        TRUSTED_RETURN(-1);
    }

    if (*len % 4 != 0) {
        TRUSTED_RETURN(-1);
    }

    uint64_t private_handle = 0;
    if (get_private_handle(public_handle, &private_handle)) {
        TRUSTED_RETURN(-1);
    }

    uint32_t status = _mmio_read_plain(fbytes, len, private_handle);
    int32_t ret = (status == GATEKEEPER_EOK) ? 0 : -1;
    TRUSTED_RETURN(ret);
}

uint64_t _mmio_decrypt_file(uint8_t* fbytes,
                            uint8_t* encrypted,
                            uint32_t* len,
                            uint32_t* tag,
                            uint64_t private_handle)
{
    _mmio_write_qw_lohi(MMIO_BASE, REG_PRIV_HANDLE_LO, REG_PRIV_HANDLE_HI, private_handle);

    _mmio_write_dw(MMIO_BASE, REG_TAG0, tag[0]);
    _mmio_write_dw(MMIO_BASE, REG_TAG1, tag[1]);
    _mmio_write_dw(MMIO_BASE, REG_TAG2, tag[2]);
    _mmio_write_dw(MMIO_BASE, REG_TAG3, tag[3]);

    _mmio_write_dw(MMIO_BASE, REG_LENDW, *len / 4);
    _mmio_write_dw(MMIO_BASE, REG_MODE, MODE_READ_ENCRYPT);

    for (uint32_t i = 0; i < *len / 4; ++i) {
        _mmio_write_dw(MMIO_BASE, REG_FILEDW, ((uint32_t*)encrypted)[i]);
    }

    if (_mmio_read_dw(MMIO_BASE, REG_STATUS) != GATEKEEPER_EOK) {
        return (uint64_t)-1;
    }

    uint32_t tmp_len = _mmio_read_dw(MMIO_BASE, REG_LENDW);
    tmp_len *= 4;

    if (tmp_len > *len) {
        return (uint64_t)-1;
    }

    *len = tmp_len;

    for (uint32_t i = 0; i < *len / 4; ++i) {
        uint32_t* dw_ptr = (uint32_t*)fbytes + i;
        *dw_ptr = _mmio_read_dw(MMIO_BASE, REG_FILEDW);
    }

    return _mmio_read_dw(MMIO_BASE, REG_STATUS);
}

TRUSTED_API(.READ_FILE_ENC)
int32_t read_file_enc(uint64_t public_handle, uint32_t* tag, uint8_t* fbytes, uint32_t* len)
{
    TRUSTED_MAYBE_FORCE_NONLEAF();

    if (fbytes == NULL || len == NULL || tag == NULL) {
        TRUSTED_RETURN(-1);
    }

    if (*len % 4 != 0) {
        TRUSTED_RETURN(-1);
    }

    uint64_t private_handle = 0;
    if (get_private_handle(public_handle, &private_handle)) {
        TRUSTED_RETURN(-1);
    }

    uint8_t encrypted[MAX_FILE_SIZE] = { 0 };
    uint32_t enc_sz = *len + 12;
    uint64_t read_status = _mmio_read_plain(encrypted, &enc_sz, private_handle);
    if (read_status != GATEKEEPER_EOK) {
        TRUSTED_RETURN(-1);
    }

    uint64_t status = _mmio_decrypt_file(fbytes, encrypted, &enc_sz, tag, private_handle);
    if (status != GATEKEEPER_EOK) {
        TRUSTED_RETURN(-1);
    }

    *len = enc_sz;
    TRUSTED_RETURN(0);
}

void putc(char c)
{
    _mmio_write_b(UART0_BASE, 0, c);
}

void out_str(const char* s)
{
    if (!s) { // print "(null)" on NULL
        const char* n = "(null)";
        while (*n)
            putc(*n++);
        return;
    }
    while (*s)
        putc(*s++);
}

void out_u64_base(uint64_t v, uint32_t base)
{
    // @VULN
    char buf[16] = { 0 };
    const char* digits = "0123456789abcdef";
    int32_t i = 0;

    if (base < 2 || base > 16)
        return;

    if (v == 0) {
        putc('0');
        return;
    }

    while (v) {
        buf[i++] = digits[v % base];
        v /= base;
    }
    buf[i] = '\0';

    while (i--)
        putc(buf[i]);
}

void out_i64_dec(int64_t x)
{
    if (x < 0) {
        putc('-');
        // cast to uint32_t to avoid overflow on INT64_MIN
        out_u64_base((uint64_t)(-(x + 1)) + 1u, 10);
    } else {
        out_u64_base((uint64_t)x, 10);
    }
}

void out_hex_fixed(uint64_t v, uint32_t nibble_count)
{
    // zero-padded fixed-width lowercase hex (used for %p)
    for (int32_t i = (int32_t)nibble_count - 1; i >= 0; --i) {
        uint32_t nib = (uint32_t)((v >> (i * 4)) & 0xF);
        char c = (char)(nib < 10 ? ('0' + nib) : ('a' + (nib - 10)));
        putc(c);
    }
}

TRUSTED_API(.MINI_PRINTF) void mini_printf(const char* fmt, ...)
{
    TRUSTED_MAYBE_FORCE_NONLEAF();

    va_list ap;
    va_start(ap, fmt);

    while (*fmt) {
        if (*fmt != '%') {
            putc(*fmt++);
            continue;
        }

        // '%'
        fmt++;
        if (*fmt == '%') {
            putc('%');
            fmt++;
            continue;
        }

        // Handle %llx specially (length modifier)
        if (fmt[0] == 'l' && fmt[1] == 'l' && fmt[2] == 'x') {
            uint64_t v = va_arg(ap, uint64_t);
            out_u64_base((uint64_t)v, 16);
            fmt += 3;
            continue;
        }

        // Basic specifiers
        switch (*fmt) {
            case 'b': {
                uint64_t v = va_arg(ap, uint64_t);
                out_u64_base(v, 2);
                fmt++;
                break;
            }
            case 'x': {
                uint32_t v = va_arg(ap, uint32_t);
                out_u64_base((uint64_t)v, 16);
                fmt++;
                break;
            }
            case 'd': {
                int32_t v = va_arg(ap, int32_t);
                out_i64_dec((int64_t)v);
                fmt++;
                break;
            }
            case 'c': {
                int32_t v = va_arg(ap, int32_t);
                putc((char)v);
                fmt++;
                break;
            }
            case 's': {
                const char* s = va_arg(ap, const char*);
                out_str(s);
                fmt++;
                break;
            }
            case 'p': {
                void* p = va_arg(ap, void*);
                uintptr_t u = (uintptr_t)p;
                putc('0');
                putc('x');
                uint32_t nibbles = (uint32_t)(sizeof(uintptr_t) * 2);
                out_hex_fixed((uint64_t)u, nibbles);
                fmt++;
                break;
            }
            default:
                // Unknown specifier: print it literally to make debugging easier
                putc('%');
                if (*fmt)
                    putc(*fmt++);
                break;
        }
    }

    va_end(ap);
    TRUSTED_RETURN_VOID();
}
