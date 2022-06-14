#!/usr/bin/env bash

export PATH="/opt/homebrew/bin:${PATH}"
flock ~/.mail-fetch.lock ~/dotfiles/do_sync.sh
