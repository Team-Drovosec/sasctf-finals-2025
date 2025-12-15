#include "trusted_lib_api.h"

__attribute__((noinline)) void safe_hex_encode_decode_test()
{
    char data_to_encode[] = "Hello, World! This is a test string for hex encoding!";
    uint8_t symbolic_buffer[128];
    uint32_t len = sizeof(symbolic_buffer);

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, symbolic_buffer, &len);

    // Symbolize data_to_encode based on symbolic_buffer
    for (uint32_t i = 0; i < sizeof(data_to_encode); i++) {
        data_to_encode[i] ^= symbolic_buffer[i % sizeof(symbolic_buffer)];
    }

    char encoded[256] = {};
    hex_encode(data_to_encode, sizeof(data_to_encode), encoded, 0);

    mini_printf("Hex encoded data: %s\n", encoded);

    // Now decode it back
    uint8_t decoded[128] = {};
    hex_decode(encoded, sizeof(data_to_encode) * 2, decoded, sizeof(decoded));

    mini_printf("Decoded data: %s\n", decoded);
}

ENTRY_POINT_ATTR void start()
{
    safe_hex_encode_decode_test();
    ENTRY_POINT_END();
}
