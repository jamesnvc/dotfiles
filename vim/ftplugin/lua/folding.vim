if &filetype != 'lua'
  finish
endif

setlocal foldmethod=expr
setlocal foldexpr=GetLuaFold(v:lnum)

let s:lua_fold_begin = ['function', 'if', 'for', 'repeat', 'while']
let s:lua_fold_end = ['end', 'until']
let s:lua_begin_regex = '\v(--.*)@<!<(' . join(s:lua_fold_begin, '|') . ')>'
let s:lua_end_regex = '\v(--.*)@<!<(' . join(s:lua_fold_end, '|') . ')>'

function! GetLuaFold(lnum)
  let line = getline(a:lnum)
  if line =~# s:lua_begin_regex
    if line =~# s:lua_end_regex
      return '='
    endif
    return 'a1'
  "elseif line =~# '\v<(else(if)?)>'
  " TODO: Make else(if)? lines fold properly
    "return '>' . prevFold
  elseif line =~# s:lua_end_regex
    return 's1'
  else
    return '='
  endif
endfunction
