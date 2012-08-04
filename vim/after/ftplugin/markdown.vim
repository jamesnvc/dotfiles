function! Foldexpr_markdown(lnum)
  let l1 = getline(a:lnum)

  if l1=~ '^\s*$'
    return '='
  endif

  let l2 = getline(a:lnum + 1)
  if l2 =~ '^==\+\s*'
    " Next line underlined (level 1)
    return '>1'
  elseif l2 =~ '^--\+\s*'
    " Next line is underlined (level 2)
    return '>2'
  elseif l1 =~ '^#'
    " Current line starts with octothorpes
    return '>' . matchend(l1, '^#\+')
  elseif a:lnum == 1
    " fold any preamble
    return '>1'
  else
    return '='
  endif
endfunction

setlocal foldexpr=Foldexpr_markdown(v:lnum)
setlocal foldmethod=expr
setlocal foldenable
setlocal foldlevel=0
setlocal foldcolumn=0

setlocal spell
setlocal textwidth=80

" Insert markdown reference-style link
function! s:AddMarkdownReferenceLink() " {{
  call inputsave()
  let refLink = input("Reference label: ")
  call inputrestore()
  execute "normal f]a[".refLink."]"
  let l = line(".")
  let c = col(".")
  " Could just use ]<Space> from unimpared, but let's try to avoid
  " dependencies (bindings need Surround.vim, anyway)
  normal! Go
  normal! "+p
  execute "normal! >>I[" . refLink . "]: "
  call cursor(l, c)
endfunction

function! s:AddMarkdownReferenceLinkSel(type)
  let sel_save = &selection
  let &selection = "inclusive"

  if a:type == 'line'
    silent execute "normal '[V']S]"
  elseif a:type == 'block'
    silent execute "normal `[\<C-V>`]S]"
  else
    silent execute "normal `[v`]S]"
  endif
  let &selection = sel_save
  call <SID>AddMarkdownReferenceLink()
endfunction
" }}

" Underline the current line with "=" signs
nnoremap <buffer> <leader>_ yypVr=
nnoremap <buffer> <leader>1 I# $ #<CR><CR><Esc>
nnoremap <buffer> <leader>2 I## $ ##<CR><CR><Esc>
nnoremap <buffer> <leader>3 I### $ ###<CR><CR><Esc>
" Wrap the next word as a markdown link
nnoremap <buffer> <leader>[ :set opfunc=<SID>AddMarkdownReferenceLinkSel<CR>g@
nnoremap <buffer> <leader>[[ <leader>[iw
vnoremap <buffer> <leader>[ S]:call <SID>AddMarkdownReferenceLink()<CR>
inoremap <buffer> <C-l> <Esc>b<leader>[a
