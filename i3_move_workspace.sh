#!/usr/bin/env bash

set -euo pipefail

if [ -z "$*" ]
then
    i3_select_workspace.sh
else
    i3-msg "move container to workspace \"$(i3_select_workspace.sh $@)\"" >/dev/null
fi
