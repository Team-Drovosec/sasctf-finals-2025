from ..triton_verifier.config import VerifierConfig
from ..triton_verifier.symbolic_verifier import TritonVerifier


def test_verify_rejects_large_program():
    tmp = VerifierConfig.full_program_size
    VerifierConfig.full_program_size = 1
    verifier = TritonVerifier()

    report = verifier.verify_program(b"AB")

    VerifierConfig.full_program_size = tmp
    assert not report.passed
    assert "exceeds limit" in report.issue


def test_verify_rejects_disallowed_region_reading():
    """
    00:   fe010113                addi    sp,sp,-32
    04:   53900793                li      a5,1337
    08:   00f12023                sw      a5,0(sp)
    0c:   02a00793                li      a5,42
    10:   00f12223                sw      a5,4(sp)
    14:   00012783                lw      a5,0(sp)
    18:   00412703                lw      a4,4(sp)
    1c:   00113c23                sd      ra,24(sp)
    20:   00e787bb                addw    a5,a5,a4
    24:   00f12423                sw      a5,8(sp)
    28:   018000ef                jal     ra,0x40
    2c:   00812783                lw      a5,8(sp)
    30:   02f5053b                mulw    a0,a0,a5
    34:   00a12623                sw      a0,12(sp)
    38:   00100073                ebreak
    3c:   0000006f                j       0x3c
    40:   123457b7                lui     a5,0x12345
    44:   6787a503                lw      a0,1656(a5) # 0x12345678
    48:   00008067                ret
    """

    program = bytes.fromhex(
        "1301 01fe 9307 9053 2320 f100 9307 a002 2322 f100 8327 0100 0327 4100 233c 1100 bb87 e700 2324 f100 ef00 8001 8327 8100 3b05 f502 2326 a100 7300 1000 6f00 0000 b757 3412 03a5 8767 6780 0000"
    )

    report = TritonVerifier().verify_program(program)
    print(report)

    assert not report.passed
    assert (
        "instruction 0x0000000000300044: lw a0, 0x678(a5) reads from disallowed address 0x0000000012345678"
        in report.issue
    )


def test_verify_rejects_uninitialized_memory_reading():
    """
    __attribute__((noinline)) void read_undefined() {
        uint64_t local[0x100];

        uint64_t tmp = 0x1337133742424242;

        for (size_t i = 0; i < sizeof(local) / sizeof(local[0]); i++) {
            tmp ^= *(volatile uint64_t *)&local[i];
        }
    }

    ENTRY_POINT_ATTR void start() {
        uint8_t fbytes[2048];

        uint64_t len = sizeof(fbytes);
        read_file_plain(0, fbytes, &len);
        read_undefined();

        ENTRY_POINT_END();
    }
    """

    program = bytes.fromhex(
        """
        1301 0181 2334 117e 2330 817e 1304 017f
        1301 01fd b7f7 ffff 9387 07ff b387 8700
        3717 0000 1307 0780 23bc e77e b717 0020
        9397 2700 37f7 ffff 1307 877f 1307 07ff
        b306 8700 37f7 ffff 1307 077f 3307 e400
        1386 0600 9305 0700 1305 0000 e780 0700
        ef00 c000 7300 1000 6f00 0000 1301 0181
        2334 817e 1304 017f 1301 01fd 9707 0000
        9387 470a 83b7 0700 2334 f4fe 2330 04fe
        6f00 c003 b7f7 ffff 9387 077f 9387 07ff
        3387 8700 8337 04fe 9397 3700 b307 f700
        83b7 0700 0337 84fe b347 f700 2334 f4fe
        8337 04fe 9387 1700 2330 f4fe 0337 04fe
        9307 f00f e3f0 e7fc 1300 0000 1300 0000
        1301 0103 0334 817e 1301 017f 6780 0000
        0000 0080 0000 0000 0020 0080 0000 0000
        0040 0080 0000 0000 0060 0080 0000 0000
        0080 0080 0000 0000 00a0 0080 0000 0000
        4242 4242 3713 3713
        """
    )

    report = TritonVerifier().verify_program(program)
    print(report)

    assert not report.passed
    assert (
        "instruction 0x00000000003000b0: ld a5, 0(a5) attempted to access undefined memory at *0x000000000033efc0, size=8 = 0x00000000000000"
        in report.issue
    )


def test_verify_rejects_symbolic_pc():
    """
    __attribute__((noinline)) void overwrite_ret_with_symbolic_data() {
      uint8_t data[16];
      uint64_t len = 32;

      read_file_plain(0x1337133713371337, data, &len);
      // memset(data, 'A', 32);
    }

    __attribute__((noinline)) void proxy() { overwrite_ret_with_symbolic_data(); }

    ENTRY_POINT_ATTR void start() {
      proxy();
      ENTRY_POINT_END();
    }
    """

    program = bytes.fromhex(
        """
        1301 01ff 2334 1100 ef00 4004 7300 1000
        6f00 0000 1301 01fd 9307 0002 2334 f100
        b717 0020 2334 1102 1306 8100 9305 0101
        1705 0000 0335 0502 9397 2700 e780 0700
        8330 8102 1301 0103 6780 0000 6ff0 9ffc
        3713 3713 3713 3713
        """
    )

    report = TritonVerifier().verify_program(program)
    print(report)

    assert not report.passed
    assert "instruction 0x0000000000000000: c.unimp PC is symbolic!" in report.issue
