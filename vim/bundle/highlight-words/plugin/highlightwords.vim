nnoremap <silent> <leader>h1 :execute 'match W1 /\<<c-r><c-w>\>/'<cr>
nnoremap <silent> <leader>h2 :execute '2match W2 /\<<c-r><c-w>\>/'<cr>
nnoremap <silent> <leader>h3 :execute '3match W3 /\<<c-r><c-w>\>/'<cr>

hi W1 guibg=#aeee00 guifg=#000000 ctermbg=154 ctermfg=16
hi W2 guibg=#00eeae guifg=#000000 ctermbg=134 ctermfg=16
hi W3 guibg=#00aeee guifg=#000000 ctermbg=124 ctermfg=16
