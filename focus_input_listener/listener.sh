#!/usr/bin/env bash
# shellcheck shell=bash
# shellcheck disable=SC3000-SC4000

set -euo pipefail

input_codepoint(){
    local char=$(perl -C -e "print chr ${1}")
    osascript <<-EOF
    tell application "System Events"
      set tmp to the clipboard
      set the clipboard to "${char}"
      keystroke "v" using command down
      delay 0.2
      set the clipboard to tmp
    end tell
EOF
}

listen() {
    SERIAL_DEVICES=(/dev/cu.usbmodem*)
    SERIAL_DEVICE="${SERIAL_DEVICES[0]}"

    echo "Listening on ${SERIAL_DEVICE}" >&2

    while IFS= read -r -d $'\n' line
    do
        IFS=' ' read -ra command <<< "$line"
        case "${command[0]}" in
            unicode_input)
                input_codepoint "${command[1]}";;
            *) echo "Unknown event ${command[0]}";;
        esac
    done < "${SERIAL_DEVICE}"
}

listen
