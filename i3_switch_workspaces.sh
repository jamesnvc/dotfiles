#!/usr/bin/env bash

set -euo pipefail

if [ -z "$*" ]
then
    i3_select_workspace.sh
else
    i3-msg workspace $(i3_select_workspace.sh "$@") >/dev/null
fi
