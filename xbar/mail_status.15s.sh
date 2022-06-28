#!/usr/bin/env bash
# <xbar.title> Notmuch mail status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>James Cash</xbar.author>
# <xbar.dependencies>notmuch</xbar.dependencies>

set -euo pipefail

NOTMUCH=/opt/homebrew/bin/notmuch
JQ=/opt/homebrew/bin/jq

unread=$("${NOTMUCH}" count -- 'tag:inbox and tag:unread')
inbox=$("${NOTMUCH}" count -- 'tag:inbox')

if [ "${unread}" -gt 0 ]; then
    printf "📥%s" "${unread}"
fi
printf "📦%s\n" "${inbox}"

echo "---"

echo "Force Fetch | shell=/Users/james/dotfiles/run_mail_standalone.sh"
$NOTMUCH search --format=json --output=summary -- tag:inbox and tag:unread | \
    $JQ -r -c '.[] | .authors + " - " + .subject' | \
    tr '|' ','
