call TurnOnClojureFolding()
compiler clojure
setl foldmarker=(,)
setl wildignore+=classes,lib,.m2

augroup testSettings
  au!
  autocmd! BufEnter *_test.clj setl lispwords+=testing
augroup END

" Friendlier Paredit mappings.
"noremap <buffer> () :<c-u>call PareditWrap("(", ")")<cr>
"noremap <buffer> )( :<c-u>call PareditSplice()<cr>
"noremap <buffer> (( :<c-u>call PareditMoveLeft()<cr>
"noremap <buffer> )) :<c-u>call PareditMoveRight()<cr>
"noremap <buffer> (j :<c-u>call PareditJoin()<cr>
"noremap <buffer> (s :<c-u>call PareditSplit()<cr>
"noremap <buffer> [ :<c-u>call PareditSmartJumpOpening(0)<cr>
"noremap <buffer> ] :<c-u>call PareditSmartJumpClosing(0)<cr>
