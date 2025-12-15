#include "trusted_lib_api.h"

ENTRY_POINT_ATTR void start()
{
    volatile uint32_t* ptr = (uint32_t*)0x41414141;
    *ptr = 0xDEADBEEF;
    ENTRY_POINT_END();
}
