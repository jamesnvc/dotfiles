#!/bin/bash

MAX_DESKTOPS=10

WORKSPACES=$(seq -s '\n' 1 1 ${MAX_DESKTOPS})

EMPTY_WORKSPACE=$( (i3-msg -t get_workspaces | jq -r '.[].num' ; \
            echo -e ${WORKSPACES} ) | sort -n | uniq -u | head -n 1)

i3-msg workspace ${EMPTY_WORKSPACE}
