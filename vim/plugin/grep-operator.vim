nnoremap <leader>g :set operatorfunc=<SID>GrepOperator<CR>g@
vnoremap <leader>g :<C-u>call <SID>GrepOperator(visualmode())<CR>

function! s:GrepOperator(type)

  echom "Type = " . a:type

  let l:saved_reg = @@

  let l:cmds = {}
  l:cmds['v'] = "normal! `<v`>y"
  l:cmds['char'] = "normal! `[v`]y"

  echom "Executing " . get(l:cmds, a:type, "")
  execute get(l:cmds, a:type, "")

  silent execute "grep! " . shellescape(@@) . " ."
  copen
  let @@ = l:saved_reg
endfunction
