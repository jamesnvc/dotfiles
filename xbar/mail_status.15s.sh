#!/usr/bin/env bash
# <xbar.title> Notmuch mail status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>James Cash</xbar.author>
# <xbar.dependencies>notmuch</xbar.dependencies>

set -euo pipefail

NOTMUCH=/opt/homebrew/bin/notmuch

unread=$("${NOTMUCH}" count -- 'tag:inbox and tag:unread')
inbox=$("${NOTMUCH}" count -- 'tag:inbox')

printf "👀 %s ✉ %s\n" "${unread}" "${inbox}"
