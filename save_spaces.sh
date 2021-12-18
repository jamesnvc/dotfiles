#!/usr/bin/env bash

i3-msg -t get_workspaces | jq -r '.[].name'  | tee ~/spaces
