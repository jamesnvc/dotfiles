syn region markdownFootnote matchgroup=markdownLinkDelimiter start="\[\^" end="\]" keepend nextgroup=markdownId
hi def link markdownFootnote htmlLink
