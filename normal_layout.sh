#!/bin/bash

set -euo pipefail

XKBDIR="$HOME/.xkb"
XKBMAPFILE="$XKBDIR/keymap/normal"

xkbcomp -I"$XKBDIR" "$XKBMAPFILE" "${DISPLAY%%.*}"
