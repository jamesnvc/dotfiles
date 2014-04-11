augroup testSettings
  au!
  autocmd! BufEnter *_test.clj setl lispwords+=testing
augroup END

setlocal foldmethod=marker foldmarker=(,)
