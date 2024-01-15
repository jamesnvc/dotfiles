#!/usr/bin/env bash

set -euo pipefail

shopt -sq extglob

while IFS= read -r -d $'\0' path; do
    file=$(basename "${path}")
    esc_file=${file//,U=[0-9]*([0-9])/}
    if [[ -f "${path}" ]]; then
        mv -n "${path}" "${HOME}/.mail/fastmail/Archive/cur/${esc_file}"
        rm -f "${path}"
    fi
done < <(notmuch search --output=files --format=text0 -- folder:Inbox and not tag:inbox)

notmuch new
