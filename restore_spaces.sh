#!/usr/bin/env bash

set -euo pipefail

i3-msg 'workspace 1'

while read -r space; do
    if [ -n "${space}" ]; then
        echo "Creating space ${space}"
        i3-msg "workspace ${space}; append_layout /home/james/i3_layout.json"
    fi
done < ~/spaces
