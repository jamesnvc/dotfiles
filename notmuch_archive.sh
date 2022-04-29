#!/usr/bin/env bash

set -euo pipefail

shopt -sq extglob

while IFS= read -r -d $'\0' path; do
    file=$(basename "${path}")
    esc_file=${file//,U=[0-9]*([0-9])/}
    mv -n "${path}" "${HOME}/.mail/fastmail/Archive/cur/${esc_file}"
done < <(notmuch search --output=files --format=text0 -- folder:Inbox and not tag:inbox)

while IFS= read -r -d $'\0' path; do
    file=$(basename "${path}")
    esc_file=${file//,U=[0-9]*([0-9])/}
    cp -n "${path}" "${HOME}/.mail/fastmail/Inbox/cur/${esc_file}"
done < <(notmuch search --output=files --format=text0 -- tag:inbox and not folder:Inbox)

notmuch new
