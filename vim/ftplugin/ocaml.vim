" Setting up merlin
" This is just in vimrc now, things get screwed up if we try to load it in
" ftplugin
"if executable('ocamlmerlin') && has('python')
  "let s:ocamlmerlin = substitute(system('opam config var share'), '\n$', '', '''') . "/ocamlmerlin"
  "execute "set rtp+=".s:ocamlmerlin."/vim"
  "let g:syntastic_ocaml_checkers = ['merlin']
"endif


let s:ocp_indent = substitute(system("opam config var share"), '\n$', '', '') . "/vim/syntax/ocp-indent.vim"
autocmd FileType ocaml exec ":source " . s:ocp_indent

":execute "helptags " . substitute(system('opam config var share'),'\n$','','''') .  "/ocamlmerlin/vim/doc"
