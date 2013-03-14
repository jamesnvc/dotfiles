if &filetype != 'lua'
  finish
endif

"setlocal foldmethod=marker
setlocal foldmethod=expr
setlocal foldexpr=GetLuaFold(v:lnum)

function! GetLuaFold(lnum)
  let prevFold = max([0, foldlevel(a:lnum - 1)])
  let line = getline(a:lnum)
  if line =~# '\v(--.*)@<!<(function|if|for|repeat|while)>'
    return 'a1'
  "elseif line =~# '\v<(else(if)?)>'
  " TODO: Make else(if)? lines fold properly
    "return '>' . prevFold
  elseif line =~# '\v<(end|until)>'
    return 's1'
  else
    return '='
  endif
endfunction
