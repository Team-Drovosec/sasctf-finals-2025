#include "trusted_lib_api.h"

__attribute__((noinline)) void unsafe_symptr_might_point_to_uninitialized_data_multilayer()
{
    uint64_t data[16];
    uint32_t len = sizeof(data);
    uint8_t buf[128] = { 0 };
    char* uninit_ptr = (char*)&data - 0x2000;

    TRUSTED_CALL(read_file_plain, 0x1337133713371337, (uint8_t*)data, &len);

    const uint32_t* inner_ptrs0[0x10] = { (void*)&buf,      (void*)&buf,       (void*)&buf,
                                          (void*)&buf,      (void*)uninit_ptr, (void*)uninit_ptr,
                                          (void*)uninit_ptr };
    const uint32_t* inner_ptrs1[0x10] = { (void*)&buf, (void*)&buf, (void*)&buf, (void*)&buf,
                                          (void*)&buf, (void*)&buf, (void*)&buf };
    const uint32_t* inner_ptrs2[0x10] = { (void*)uninit_ptr, (void*)&buf, (void*)&buf, (void*)&buf,
                                          (void*)&buf,       (void*)&buf, (void*)&buf };

    const uint32_t** ptrs[] = { inner_ptrs0, inner_ptrs1, inner_ptrs2 };

    uint32_t symbolic_index_outer0 = verifier_ranged_u32(*(volatile uint32_t*)&data[0], 1, 2);
    uint32_t symbolic_index_outer1 = verifier_ranged_u32(*(volatile uint32_t*)&data[4], 0, 4);

    const uint32_t** p = ptrs[symbolic_index_outer0];
    const uint32_t* final_ptr = p[symbolic_index_outer1];
    [[maybe_unused]] const volatile uint32_t v =
        *(volatile uint32_t*)final_ptr; // this one should be marked as symbolic,
                                        // and unsafe, as it might point to
                                        // uninitialized data!
}

ENTRY_POINT_ATTR void start()
{
    unsafe_symptr_might_point_to_uninitialized_data_multilayer();
    ENTRY_POINT_END();
}
