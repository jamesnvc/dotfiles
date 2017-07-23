#!/bin/bash

set -euo pipefail

XKBDIR="$HOME/.xkb"
XKBMAPFILE="$XKBDIR/keymap/custom"

xkbcomp -I"$XKBDIR" "$XKBMAPFILE" "${DISPLAY%%.*}"
