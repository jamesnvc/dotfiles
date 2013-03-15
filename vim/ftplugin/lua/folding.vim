if &filetype != 'lua'
  finish
endif

setlocal foldmethod=expr
setlocal foldexpr=GetLuaFold(v:lnum)


let s:lua_fold_begin = ['function', 'if', 'for', 'repeat', 'while']
let s:lua_fold_end = ['end', 'until']
let s:lua_begin_regex = '\v(--.*)@<!<(' . join(lua_fold_begin, '|') . ')>'
let s:lua_end_regex = '\v(--.*)@<!<(' . join(lua_fold_end, '|') . ')>'

function! IsMatchCode(lnum, line, regexp)
  let col = match(a:line, a:regexp)
  let syns = map(synstack(a:lnum, col), 'synIDattr(v:val, "name")')
  if type(syns) != type([])
    return 1
  else
    return count(syns, 'luaString') == 0 && count(syns, 'luaComment') == 0
  endif
endfunction

function! GetLuaFold(lnum)
  let line = getline(a:lnum)
  if line =~# s:lua_begin_regex
    if line =~# s:lua_end_regex || !IsMatchCode(a:lnum, line, s:lua_begin_regex)
      return '='
    endif
    return 'a1'
  "elseif line =~# '\v<(else(if)?)>'
  " TODO: Make else(if)? lines fold properly
    "return '>' . prevFold
  elseif line =~# s:lua_end_regex && IsMatchCode(a:lnum, line, s:lua_end_regex)
    return 's1'
  else
    return '='
  endif
endfunction
