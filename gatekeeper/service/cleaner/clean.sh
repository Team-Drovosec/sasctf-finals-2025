#!/bin/sh

while true; do
    find /var/gatekeeper/executor/ -maxdepth 2 -type f -mmin +15 \
    ! -name "enc_key.bin" \
    -print -delete

    sleep 200
done