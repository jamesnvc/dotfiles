setl foldmethod=syntax
setl spell
setl textwidth=80

" Insert markdown reference-style link
function! AddMarkdownReferenceLink() " {{
  call inputsave()
  let refLink = input("Reference label: ")
  call inputrestore()
  exe "normal f]a[".refLink."]"
  let l = line(".")
  let c = col(".")
  " Could just use ]<Space> from unimpared, but let's try to avoid
  " dependencies (bindings need Surround.vim, anyway)
  normal! Go
  normal! "+p
  exe "normal! >>I[" . refLink . "]: "
  call cursor(l, c)
endfunction

function! AddMarkdownReferenceLinkSel(type)
  let sel_save = &selection
  let &selection = "inclusive"

  if a:type == 'line'
    silent exe "normal '[V']S]"
  elseif a:type == 'block'
    silent exe "normal `[\<C-V>`]S]"
  else
    silent exe "normal `[v`]S]"
  endif
  let &selection = sel_save
  call AddMarkdownReferenceLink()
endfunction
" }}

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
