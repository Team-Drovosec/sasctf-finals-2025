LIB_BASE   = 0x80000000; /* user-code: 0x300000 */
MAX_LIB_SIZE = 0x00010000; /* 64 KiB */

CREATE_FILE_PLAIN = 0x80000000;
CREATE_FILE_ENC   = 0x80002000;
READ_FILE_PLAIN   = 0x80004000;
READ_FILE_ENC     = 0x80006000;
MINI_PRINTF      = 0x8000A000;

SECTIONS
{
       . = LIB_BASE;

       . = CREATE_FILE_PLAIN;
       .CREATE_FILE_PLAIN : { KEEP(*(.CREATE_FILE_PLAIN)) }
       ASSERT(ADDR(.CREATE_FILE_PLAIN) == CREATE_FILE_PLAIN, "CREATE_FILE_PLAIN drifted")

       . = CREATE_FILE_ENC;
       .CREATE_FILE_ENC : { KEEP(*(.CREATE_FILE_ENC)) }
       ASSERT(ADDR(.CREATE_FILE_ENC) == CREATE_FILE_ENC, "CREATE_FILE_ENC drifted")

       . = READ_FILE_PLAIN;
       .READ_FILE_PLAIN : { KEEP(*(.READ_FILE_PLAIN)) }
       ASSERT(ADDR(.READ_FILE_PLAIN) == READ_FILE_PLAIN, "READ_FILE_PLAIN drifted")

       . = READ_FILE_ENC;
       .READ_FILE_ENC : { KEEP(*(.READ_FILE_ENC)) }
       ASSERT(ADDR(.READ_FILE_ENC) == READ_FILE_ENC, "READ_FILE_ENC drifted")

       . = MINI_PRINTF;
       .MINI_PRINTF : { KEEP(*(.MINI_PRINTF)) }
       ASSERT(ADDR(.MINI_PRINTF) == MINI_PRINTF, "MINI_PRINTF drifted")

       . = ALIGN(4);
       .text   ALIGN(4) : { *(.text .text.*) }
       .rodata ALIGN(4) : { *(.rodata .rodata.*) }

       . = ALIGN(16);
       ASSERT(. <= LIB_BASE + MAX_LIB_SIZE, "code+ro exceed 64 KiB window!")
/* 
       . = DATA_BASE;

       .data ALIGN(16) : { *(.sdata .sdata.*) *(.data .data.*) }
       .bss (NOLOAD) : { *(.sbss .sbss.*) *(.bss .bss.*) *(COMMON) } */

       /* __text_start = ADDR(.text);
       __text_end   = .;
       __data_start = ADDR(.data);
       __data_end   = .;
       __bss_start  = ADDR(.bss);
       __bss_end    = .; */
}
