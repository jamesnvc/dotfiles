#!/bin/bash

set -euo pipefail

if [ -z "$*" ]
then
    echo EMPTY
    i3-msg -t get_workspaces | jq -r '.[].name' | sort -n
else
    WORKSPACE=$*

    if [ x"EMPTY" = x"${WORKSPACE}" ]
    then
        i3_empty_workspace.sh >/dev/null
    elif [ -n "${WORKSPACE}" ]
    then
        i3-msg workspace "${WORKSPACE}" >/dev/null
    fi
fi
