.text
.align 2

.global __trusted_scrub_after_epilogue_keep_a0
.type __trusted_scrub_after_epilogue_keep_a0, @function

__trusted_scrub_after_epilogue_keep_a0:
    /* Scrub caller-saved registers to avoid leaking info, but keep a0 */
    mv   ra, a1        # keep final target in ra
    # zero caller-saved regs except a0 (return) and ra (needed to jump)
    mv   a1, zero
    mv   a2, zero
    mv   a3, zero
    mv   a4, zero
    mv   a5, zero
    mv   a6, zero
    mv   a7, zero

    mv   t0, zero
    mv   t1, zero
    mv   t2, zero
    mv   t3, zero
    mv   t4, zero
    mv   t5, zero
    mv   t6, zero
    jalr x0, 0(ra)

    .size __trusted_scrub_after_epilogue_keep_a0, .-__trusted_scrub_after_epilogue_keep_a0