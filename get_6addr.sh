#!/usr/bin/env bash

ip -6 addr show dev br1 | grep -E -m 1 -o 'inet6 [0-9a-f:]+' | cut -d' ' -f2 \
    | ssh touchdowntunes 'cat > ~/gonk_addr'
