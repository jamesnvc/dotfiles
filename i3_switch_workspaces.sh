#!/bin/bash

if [ -z $@ ]
then
function gen_workspaces()
{
    i3-msg -t get_workspaces | jq -r '.[].name' | sort -n
}


echo empty; gen_workspaces
else
    WORKSPACE=$@

    if [ x"empty" = x"${WORKSPACE}" ]
    then
        i3_empty_workspace.sh >/dev/null
    elif [ -n "${WORKSPACE}" ]
    then
        i3-msg workspace "${WORKSPACE}" >/dev/null
    fi
fi
