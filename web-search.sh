#!/usr/bin/env bash

# -----------------------------------------------------------------------------
# Info:
#   author:    Miroslav Vidovic
#   file:      web-search.sh
#   created:   24.02.2017.-08:59:54
#   revision:  ---
#   version:   1.0
# -----------------------------------------------------------------------------
# Requirements:
#   rofi
# Description:
#   Use rofi to search the web.
# Usage:
#   web-search.sh
# -----------------------------------------------------------------------------
# Script:

declare -A URLS

URLS=(
  ["duckduckgo"]="https://www.duckduckgo.com/?q="
  ["google"]="https://www.google.com/search?q="
  ["bing"]="https://www.bing.com/search?q="
  ["yahoo"]="https://search.yahoo.com/search?p="
  ["github"]="https://github.com/search?q="
  ["stackoverflow"]="http://stackoverflow.com/search?q="
  ["superuser"]="http://superuser.com/search?q="
  ["askubuntu"]="http://askubuntu.com/search?q="
  ["imdb"]="http://www.imdb.com/find?ref_=nv_sr_fn&q="
  ["rottentomatoes"]="https://www.rottentomatoes.com/search/?search="
  ["youtube"]="https://www.youtube.com/results?search_query="
)

# List for rofi
gen_list() {
    for i in "${!URLS[@]}"
    do
      echo "$i"
    done
}

main() {
    while getopts p: o; do
        case "${o}" in
            p)
                platform=${OPTARG}
                ;;
            *)
                ;;
        esac
    done
    shift $((OPTIND-1))
    if [ -z "${platform}" ]; then
        # Pass the list to rofi
        platform=$( (gen_list) | rofi -dmenu -matching fuzzy -only-match -location 0 -p "Search > " )
    fi

    query=$( (echo ) | rofi  -dmenu -matching fuzzy -location 0 -p "Query > " )
    if [[ -n "$query" ]]; then
        url=${URLS[$platform]}$query
        xdg-open "$url"
    fi
}

main "$@"

exit 0
