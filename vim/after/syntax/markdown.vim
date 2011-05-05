syn region markdownFootnote matchgroup=markdownLinkDelimiter start="\[\^" end="\]" keepend nextgroup=markdownId
hi def link markdownFootnote htmlLink
syn region markdownFold start="^\z(#\+\) " end="\(^#\(\z1#*\)\@!#*[^#]\)\@=" transparent fold
