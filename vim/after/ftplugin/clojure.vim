call TurnOnClojureFolding()
compiler clojure
let vimclojure#HighlightBuiltins = 1
let vimclojure#ParenRainbow = 1
let vimclojure#WantNailgun = 0  " Don't start the repl
let vimclojure#NailgunClient = "/usr/local/bin/ng"
setlocal foldmarker=(,)

nnoremap <buffer> <localleader>ef :<C-u>call SlimvEvalExp()<CR>
nnoremap <buffer> <localleader>ee :<C-u>call SlimvEvalDefun()<CR>
nmap <buffer> \i \di

