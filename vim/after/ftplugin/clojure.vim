call TurnOnClojureFolding()
compiler clojure
setl foldmarker=(,)
setl wildignore+=classes,lib,.m2

augroup testSettings
  au!
  autocmd! BufEnter *_test.clj setl lispwords+=testing
augroup END
