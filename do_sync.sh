#!/usr/bin/env bash

set -euo pipefail

"${HOME}/dotfiles/notmuch_archive.sh"
mbsync -Va
notmuch new

#pkill -RTMIN+1 i3blocks
