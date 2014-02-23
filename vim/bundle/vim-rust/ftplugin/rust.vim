" Vim syntax file
" Language:     Rust
" Maintainer:   Chris Morgan <me@chrismorgan.info>
" Last Change:  2013 Jul 10

if exists("b:did_ftplugin")
	finish
endif
let b:did_ftplugin = 1

" The rust source code at present seems to typically omit a leader on /*!
" comments, so we'll use that as our default, but make it easy to switch.
" This does not affect indentation at all (I tested it with and without
" leader), merely whether a leader is inserted by default or not.
if exists("g:rust_bang_comment_leader") && g:rust_bang_comment_leader == 1
	" Why is the `,s0:/*,mb:\ ,ex:*/` there, you ask? I don't understand why,
	" but without it, */ gets indented one space even if there were no
	" leaders. I'm fairly sure that's a Vim bug.
	setlocal comments=s1:/*,mb:*,ex:*/,s0:/*,mb:\ ,ex:*/,:///,://!,://
else
	setlocal comments=s0:/*!,m:\ ,ex:*/,s1:/*,mb:*,ex:*/,:///,://!,://
endif
setlocal commentstring=//%s
setlocal formatoptions-=t formatoptions+=croqnl
" j was only added in 7.3.541, so stop complaints about its nonexistence
silent! setlocal formatoptions+=j

" This includeexpr isn't perfect, but it's a good start
setlocal includeexpr=substitute(v:fname,'::','/','g')

" NOT adding .rc as it's being phased out (0.7)
setlocal suffixesadd=.rs

if exists("g:ftplugin_rust_source_path")
    let &l:path=g:ftplugin_rust_source_path . ',' . &l:path
endif

if exists("g:loaded_delimitMate")
	if exists("b:delimitMate_excluded_regions")
		let b:rust_original_delimitMate_excluded_regions = b:delimitMate_excluded_regions
	endif
	let b:delimitMate_excluded_regions = delimitMate#Get("excluded_regions") . ',rustLifetimeCandidate,rustGenericLifetimeCandidate'
endif

let b:undo_ftplugin = "setlocal formatoptions< comments< commentstring< includeexpr< suffixesadd< | if exists('b:rust_original_delimitMate_excluded_regions') | let b:delimitMate_excluded_regions = b:rust_original_delimitMate_excluded_regions | unlet b:rust_original_delimitMate_excluded_regions | elseif exists('b:delimitMate_excluded_regions') | unlet b:delimitMate_excluded_regions | endif"
