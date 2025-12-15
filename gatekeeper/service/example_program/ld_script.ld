CODE_BASE   = 0x300000; /* user-code: 0x300000 */
DATA_OFFSET = 0x00010000;
DATA_BASE   = CODE_BASE + DATA_OFFSET; /* user-data: 0x310000 */
DATA_SIZE   = 0x00010000; /* 64 KiB */

SECTIONS
{
       . = CODE_BASE;

       .entry  ALIGN(4) : { KEEP(*(.entry)) }
       .text   ALIGN(4) : { *(.text .text.*) }
       .rodata ALIGN(4) : { *(.rodata .rodata.*) }

       . = ALIGN(16);
       ASSERT(. <= DATA_BASE, "code+ro exceed 64 KiB window; increase DATA_OFFSET")

       . = DATA_BASE;

       .data ALIGN(16) : { *(.sdata .sdata.*) *(.data .data.*) }
       .bss (NOLOAD) : { *(.sbss .sbss.*) *(.bss .bss.*) *(COMMON) }

       . = ALIGN(16);
       ASSERT(. <= DATA_BASE + DATA_SIZE, "data exceed 64 KiB window; increase DATA_SIZE")

       __text_start = ADDR(.text);
       __text_end   = .;
       __data_start = ADDR(.data);
       __data_end   = .;
       __bss_start  = ADDR(.bss);
       __bss_end    = .;
}
