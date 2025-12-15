#include "trusted_lib_api.h"

__attribute__((noinline)) void safe_call_all_apis()
{
    uint8_t fbytes_plain[16] = { 0 };
    uint32_t len_plain = sizeof(fbytes_plain);

    uint64_t handle = TRUSTED_CALL(create_file_plain, fbytes_plain, sizeof(fbytes_plain));
    TRUSTED_CALL(read_file_plain, handle, fbytes_plain, &len_plain);

    uint8_t fbytes_enc[16] = { 0 };
    uint32_t len_enc = sizeof(fbytes_enc);
    uint32_t iv[4] = { 0 };
    uint32_t tag[4] = { 0 };

    uint64_t handle_enc = TRUSTED_CALL(create_file_enc, fbytes_enc, sizeof(fbytes_enc), iv, tag);
    TRUSTED_CALL(read_file_enc, handle_enc, tag, fbytes_enc, &len_enc);
}

ENTRY_POINT_ATTR void start()
{
    safe_call_all_apis();
    ENTRY_POINT_END();
}
