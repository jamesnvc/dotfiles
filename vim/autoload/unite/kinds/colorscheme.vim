let s:kind = {
      \ 'name': 'colorscheme',
      \ 'default_action': 'execute',
      \ 'action_table': {},
      \ 'parents': [],
      \ }

let s:kind.action_table.execute = {
      \ 'is_selectable': 1,
      \ }

function! s:kind.action_table.execute.func(candidates)
  if len(a:candidates) != 1
    echo "candidates must be only one"
    return
  endif
  execute "colorscheme" a:candidates[0].word
endfunction

function! unite#kinds#colorscheme#define()
  return s:kind
endfunction
