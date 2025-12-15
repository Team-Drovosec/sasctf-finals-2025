# Building Instructions

## Qemu

Building QEMU binary (v10.1.0):

1. `git clone https://github.com/qemu/qemu/`
2. `cd qemu && git checkout v10.1.0`
3. Apply the gatekeeper support patch: `git apply ../qemu-v10.1.0.patch`
4. Build with the following commands:

    ```bash
    ./configure --static --target-list=riscv64-softmmu \
        --disable-tools --disable-guest-agent \
        --disable-sdl --disable-gtk --disable-opengl --disable-virglrenderer \
        --disable-vnc --disable-spice \
        --disable-curl --disable-gnutls \
        --disable-slirp --disable-passt --disable-auth-pam \
        --disable-alsa --disable-pa --disable-oss --disable-sndio --audio-drv-list=default \
        --disable-libssh --disable-libudev --disable-xkbcommon --disable-fuse --disable-gio

    ninja -C build qemu-system-riscv64
    ```
