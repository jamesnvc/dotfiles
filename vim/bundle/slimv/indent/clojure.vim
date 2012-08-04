" clojure.vim:
"               Clojure indent plugin for Slimv
" Version:      0.9.5
" Last Change:  21 Feb 2012
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  Load Once:
if exists("b:did_indent") || exists("g:slimv_disable_clojure")
   finish
endif

runtime indent/**/lisp.vim

setlocal nolisp
setlocal autoindent
setlocal expandtab
setlocal indentexpr=SlimvIndent(v:lnum)

