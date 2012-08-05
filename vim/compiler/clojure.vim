if exists("current_compiler")
  finish
endif
let current_compiler = "clojure"

let s:cpo_save = &cpo
set cpo-=C

let &l:makeprg=fnameescape(globpath(&runtimepath, 'compiler/cake-test-wrapper.py'))

setlocal errorformat=%f:%l:%m

let &cpo = s:cpo_save
unlet s:cpo_save
