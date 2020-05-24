#!/usr/bin/env bash

set -euo pipefail

MAX_DESKTOPS=10

WORKSPACES=$(seq -s '\n' 1 1 ${MAX_DESKTOPS})

EMPTY_WORKSPACE=$( (i3-msg -t get_workspaces | jq -r '.[].num' ; \
            printf "%b" "${WORKSPACES}" ) | sort -n | uniq -u | head -n 1)

echo "${EMPTY_WORKSPACE}"
