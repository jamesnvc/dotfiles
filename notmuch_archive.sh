#!/usr/bin/env bash

set -euo pipefail

notmuch search --output=files --format=text0 -- folder:Inbox and not tag:inbox | xargs -0 -I'{}' mv -n '{}' "${HOME}/.mail/gmail/[Gmail]/All Mail/"
notmuch new
