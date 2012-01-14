syn sync fromstart
syn region markdownFold start="^\z(#\+\) " end="\ze\n\(#\(\z1#*\)\@!#*[^#]\)\@="re=s transparent fold keepend extend
syn region markdownFootnote matchgroup=markdownLinkDelimiter start="\[\^" end="\]" keepend nextgroup=markdownId
hi def link markdownFootnote htmlLink
