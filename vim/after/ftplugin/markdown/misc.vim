setlocal spell
setlocal textwidth=0

let b:syntastic_always_populate_loc_list = 1

" Underline the current line with "=" signs
nnoremap <buffer> <leader>_ yypVr=
nnoremap <buffer> <leader>1 I# $ #<CR><CR><Esc>
nnoremap <buffer> <leader>2 I## $ ##<CR><CR><Esc>
nnoremap <buffer> <leader>3 I### $ ###<CR><CR><Esc>
