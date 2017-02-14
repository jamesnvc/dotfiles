if exists('g:loaded_syntastic_clojure_joker_checker')
    finish
endif
let g:loaded_syntastic_clojure_joker_checker = 1

if !exists('g:syntastic_clojure_joker_sort')
    let g:syntastic_clojure_joker_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_clojure_joker_IsAvailable() dict
    return executable(self.getExec())
endfunction

function! SyntaxCheckers_clojure_joker_GetLocList() dict
    let makeprg = self.makeprgBuild({'args': '--lint'})

    let errorformat = '%f:%l:%c:%m'

    let env = {}

    return SyntasticMake({ 'makeprg': makeprg, 'errorformat': errorformat, 'env': env })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
            \ 'filetype': 'clojure',
            \ 'name': 'joker',
            \ 'exec': 'joker' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
