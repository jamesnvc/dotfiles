#!/usr/bin/zsh

if [ -s ~/.Xmodmap ]; then
  xmodmap ~/.Xmodmap
fi

XKBDIR="$HOME/.xkb"
XKBMAPFILE="$XKBDIR/keymap/custom"

xkbcomp -I"$XKBDIR" "$XKBMAPFILE" "${DISPLAY%%.*}"

wine "C:\Program Files (x86)\1Password 4\Agile1pAgent.exe" &

xcape -e 'Shift_L=parenleft;Shift_R=parenright;Control_L=Escape'

# vim: ft=zsh
