setl foldmethod=syntax
setl spell
setl textwidth=80
" Underline the current line with "=" signs
map <buffer> <leader>_ yypVr=
map <buffer> <leader>1 I# $ #<CR><CR><Esc>
map <buffer> <leader>2 I## $ ##<CR><CR><Esc>
map <buffer> <leader>3 I### $ ###<CR><CR><Esc>
" Wrap the next word as a markdown link
nmap <buffer> <leader>[ :set opfunc=AddMarkdownReferenceLinkSel<CR>g@
nmap <buffer> <leader>[[ <leader>[iw
vmap <buffer> <leader>[ S]:call AddMarkdownReferenceLink()<CR>
imap <buffer> <C-l> <Esc>b<leader>[a
