" Modeline and Notes {{
" vim: set foldmarker={{,}} foldlevel=0 foldmethod=marker :
"
"   James Cash's vimrc
"
" }}


" ***** Setup pathogen for loading bundles ***** {{
runtime! autoload/pathogen.vim
if exists('g:loaded_pathogen')
  call pathogen#runtime_append_all_bundles()
  call pathogen#helptags()
end
" }}


" ***** Basic settings ***** {{
set nocompatible
set encoding=utf-8
let mapleader = " "

colorscheme ir_black

filetype on
filetype plugin on
filetype indent on

syntax on
" }}


" ***** Set stuff ***** {{
set autoindent
set backspace=indent,eol,start
set conceallevel=2 " Enable 'Conceal' mode
set completeopt=longest,menuone,preview
set cursorline
set cryptmethod=blowfish
set expandtab
set foldenable
set foldmethod=marker
set formatoptions+=n  " gq recognizes numbered lists
set gdefault  " Make substitute global by default
set grepprg=ack\ -a\ -H\ --nocolor\ --nogroup
set hidden  " When opening a new file hide the current instead of closing it
set history=1000
set hlsearch
set ignorecase
set incsearch
set matchtime=3
set laststatus=2
set list
set listchars=tab:▸\ ,trail:¬,extends:→,nbsp:.
set magic
set nrformats+=alpha
set ofu=syntaxcomplete#Complete
set pastetoggle=<F2>
set relativenumber
set ruler
set scrolloff=2
set shiftwidth=2
set showmatch
set smartcase
set smarttab
set statusline=%n\ %y\ %F%m%r%h%w%=%{fugitive#statusline()}\ (%l,%c)\ %P
set tabstop=2
set undofile
set undolevels=1000
set visualbell
set wildmenu
set wildmode=list:longest,full
if has('gui_running')
  set guioptions-=T
  set guioptions-=m
  set guioptions-=rL
  set guicursor=a:blinkon0
endif
if has('win32')
  set shellslash
endif
" Backup stuff {{
set backup
set backupdir=$HOME/.vimbackup//
set directory=$HOME/.vimswap//
set viewdir=$HOME/.vimviews//
set undodir=$HOME/.vimundo//
" Creating backup dirs if the don't exist
if !isdirectory(expand("~/.vimbackup"))
  if has('win32')
    silent execute ' !mkdir "\%HOME\%\.vimbackup"'
    silent execute ' !mkdir "\%HOME\%\.vimswap"'
    silent execute ' !mkdir "\%HOME\%\.vimviews"'
    silent execute ' !mkdir "\%HOME\%\.vimundo"'
  else
    silent execute ' !mkdir -p $HOME/.vimbackup'
    silent execute ' !mkdir -p $HOME/.vimswap'
    silent execute ' !mkdir -p $HOME/.vimviews'
    silent execute ' !mkdir -p $HOME/.vimundo'
  endif
endif
"  }}
let g:tex_conceal="adgm"
let g:tex_flavor='latex'
let g:indent_guides_start_level=2
let g:indent_guides_guide_size=1
" }}


" ***** Define commands ***** {{
command! -nargs=0 Restore set lines=100 columns=85
command! -nargs=0 GitX !open -a GitX %:p:h<CR>
" Show syntax highlighting groups for word under cursor
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
" Delete trailing whitespace
function! CleanupWhitespace()
  let _s = @/
  let l  = line(".")
  let c  = col(".")
  keepjumps :%s/\v\s+$//e
  let @/ = _s
  call cursor(l, c)
endfunction
" Redirect a command to the clipboard
function! RedirToClipboardFunction(cmd, ...)
  let cmd = a:cmd . " " . join(a:000, " ")
  redir @*>
  exe cmd
  redir END
endfunction
command! -complete=command -nargs=+ RedirToClipboard
      \ silent! call RedirToClipboardFunction(<f-args>)
" Run jslint on the current file
function! JSLintFile()
  let lint_cmd = system("which jsl | tr -d '\n'")
  let lint_args = " -conf \"" . expand("~") . "/.jsl.conf\""
        \ . " -nologo -nofilelisting -nosummary -process \"" .
        \ expand("%") . "\""
  cexpr system(lint_cmd . lint_args)
endfunction
command! -nargs=0 JSLint call JSLintFile()
" Turn tabs into spaces from the cursor to indent.
function! TabsToSpaces()
  let l:tmp  = @/
  let l:tmp2 = @"
  " Using exe so we can insert the newline character
  exe "normal i\<CR>"
  exe "keepjumps :s/[\t]/" . repeat(" ", &tabstop) . "/"
  normal 0Dk$pjddk_
  let @/ = l:tmp
  let @" = l:tmp2
endfunction
" Ruby Commands {{
" Ruby matching strings for matchit
function! GetRubyMatchWords()
  return '\<if>:\<end\>,\<def\>:\<end\>'
endfunction

" Lists functions/methods in current file
function! g:ListRubyFunctions()
  let s:file_name = expand("%")

  exe 'vimgrep =def = ' . s:file_name

  vertical copen
  vertical resize 50

  setlocal modifiable
  silent exe '%s=' . s:file_name . '==g'
  silent exe '%s=def ==g'
  silent exe '%s=|.*|==g'
  setlocal nomodified
  setlocal nomodifiable
  setlocal nonumber
  setlocal readonly
endfunction

" Searches for function/method definition under the cursor
function! g:GotoRubyFunc()
  let find_command = 'find . -type f | grep .rb  | xargs grep -n def\ '.expand('<cword>')
  echo(find_command)
  set errorformat=%f:%l:%m
  lgetexpr system(find_command)
  rightb lopen
endfunction

" Executes spec (rspec 1.3) command in different modes
" and display results in :Error buffer
" available modes:
" - file - all specs in current file
" - line - current context or current example (cursor within context {} or it   {} block
" - all - runs whole test case
function! g:RunRspec(mode)
  "current line
  if a:mode == 'line'
    let line_num = line(".")
    let res =  system('spec -l '.line_num.' '.expand('%'))
  elseif a:mode == 'file'
    let res = system('spec '.expand('%'))
  elseif a:mode == 'all'
    let res = system('RAILS_ENV=test rake spec')
  endif
  vnew
  let e_file = tempname()
  silent execute 'e '.e_file
  put = res
  silent w | bd
  set errorformat=%f:%l:
  silent execute 'cgetfile '.e_file
  copen
endfunction

command! -bar -narg=* RRRGotoDef call g:GotoRubyFunc()
command! -bar -narg=* RRListDefs call g:ListRubyFunctions()

command! -bar -narg=0 RRSpecF call g:RunRspec('file')
command! -bar -narg=0 RRSpecL call g:RunRspec('line')
command! -bar -narg=0 RRSpecAll call g:RunRspec('all')
" }}
" TPope stuff {{
function! OpenURL(url)
  if has("win32")
    exe "!start cmd /cstart /b ".a:url.""
  elseif has("mac")
    exe "silent !open \"" . a:url . "\""
  else
    exe "silent !links \"".a:url."\""
  endif
endfunction
command! -nargs=1 OpenURL :call OpenURL(<q-args>)
" }}
" }}


" ***** Keybindings ***** {{
" Normal/operator-pending/visual-mode bindings {{
" Make navigating windows easier
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
" Toggle spellchecking
map <leader>ss :setlocal spell!<CR>
map <leader>o :BufExplorer<CR>
map <leader>C :call HexHighlight()<CR>
" Paste from clipboard
map <leader>v "+gP
map Y y$
" Undo tree
map <leader>U :GundoToggle<CR>
" Change LaTeX suite bindings from <C-j>
map <leader>J <Plug>IMAP_JumpForward
"  }}
" Normal mode bindings {{
nnoremap <leader><leader> :
nnoremap j gj
nnoremap k gk
nnoremap / /\v
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>
nmap <silent> <leader>/ :let @/=""<CR>
nmap <leader>w :w<CR>
nmap <leader>G :Gstatus<CR>
" Show syntax group
nmap <leader>P :call <SID>SynStack()<CR>
" Visually select the text last edited/pasted
nmap gV `[v`]
" Reflow paragraph
nmap Q gqgq
" Using this instead of autochdir
nmap <leader>cd :cd %:p:h<CR>
" Switch tabs to spaces for alignment purposes
nmap <leader>T :call TabsToSpaces()<CR>
" NERDTree bindings
nmap <leader>n :NERDTreeClose<CR>:NERDTreeToggle<CR>
nmap <Leader>m :NERDTreeClose<CR>:NERDTreeFind<CR>
nmap <leader>N :NERDTreeClose<CR>
nmap gt <C-w>gf
nmap gT <C-w>gF
nmap <leader><Left> :tabprevious<CR>
nmap <leader><Right> :tabnext<CR>
" Tabular
nnoremap <Leader>b= :Tabularize /=<CR>
nnoremap <Leader>b: :Tabularize /:\zs<CR>
" open URL under cursor in browser
nnoremap gb :OpenURL <cfile><CR>
nnoremap gA :OpenURL http://www.answers.com/<cword><CR>
nnoremap gG :OpenURL http://www.google.com/search?q=<cword><CR>
nnoremap gW :OpenURL http://en.wikipedia.org/wiki/Special:Search?search=<cword><CR>
"  }}
" Command-mode bindings {{
" Reopen the current file as sudo
cmap w!! w !sudo tee % > /dev/null
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap s/ s/\v
"  }}
" Visual-mode bindings {{
vmap Q gq
" Bubble multiple lines up/down using unimpared plugin.
vmap <C-Up> [egv
vmap <C-Down> ]egv
vnoremap / /\v
" Tabular
vnoremap <Leader>b= :Tabularize /=<CR>
vnoremap <Leader>b: :Tabularize /:\zs<CR>
"  }}
" Operator-pending mode bindings {{
" Next ()
onoremap <silent> inb :<C-U>normal! f(vib<cr>
onoremap <silent> anb :<C-U>normal! f(vab<cr>
onoremap <silent> in( :<C-U>normal! f(vi(<cr>
onoremap <silent> an( :<C-U>normal! f(va(<cr>
" Next {}
onoremap <silent> inB :<C-U>normal! f{viB<cr>
onoremap <silent> anB :<C-U>normal! f{vaB<cr>
onoremap <silent> in{ :<C-U>normal! f{vi{<cr>
onoremap <silent> an{ :<C-U>normal! f{va{<cr>
" Next []
onoremap <silent> ind :<C-U>normal! f[vi[<cr>
onoremap <silent> and :<C-U>normal! f[va[<cr>
onoremap <silent> in[ :<C-U>normal! f[vi[<cr>
onoremap <silent> an[ :<C-U>normal! f[va[<cr>
" Next <>
onoremap <silent> in< :<C-U>normal! f<vi<<cr>
onoremap <silent> an< :<C-U>normal! f<va<<cr>
" Next ''
onoremap <silent> in' :<C-U>normal! f'vi'<cr>
onoremap <silent> an' :<C-U>normal! f'va'<cr>
" Next ""
onoremap <silent> in" :<C-U>normal! f"vi"<cr>
onoremap <silent> an" :<C-U>normal! f"va"<cr>
"  }}
" Insert mode bindings {{
inoremap qq <Esc>
inoremap <Left> <Esc><<i
inoremap <Right> <Esc>>>i
" Make end of sentences set an undo point to facilitate typing long stretches
inoremap . .<C-g>u
inoremap ! !<C-g>u
inoremap ? ?<C-g>u
"  }}
" }}


" ***** Miscellaneous autocmds ***** {{
if has('autocmd')
  augroup cleanUp  "{{
    autocmd!
    " Delete trailing whitespace on save
    autocmd BufWritePre * :call CleanupWhitespace()
  augroup END  " }}
  augroup misc  " {{
    autocmd!
    autocmd FocusLost * wall
    " auto-chmod files with a shebang {{
    autocmd BufNewFile  * let b:chmod_exe=1
    autocmd BufWritePre * if exists("b:chmod_exe") |
          \ unlet b:chmod_exe |
          \ if getline(1) =~ '^#!' | let b:chmod_new="+x" | endif |
          \ endif
    autocmd BufWritePost,FileWritePost * if exists("b:chmod_new")|
          \ silent! execute "!chmod ".b:chmod_new." <afile>"|
          \ unlet b:chmod_new|
          \ endif
    "}}
    autocmd BufReadCmd *.jar,*.xpi call zip#Browse(expand("<amatch>"))
  augroup END  " }}
  augroup filetypes  " {{
    autocmd!
    autocmd BufRead,BufNewFile *.json setfiletype javascript
    autocmd BufRead,BufNewFile *.ru setfiletype ruby
  augroup END  "}}
  augroup completion  " {{
    autocmd!
    autocmd FileType python set omnifunc=pythoncomplete#Complete
    autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
    autocmd FileType css set omnifunc=csscomplete#CompleteCSS
  augroup END  "}}
endif
" }}


" ***** Plugin options ***** {{
" NERDTree stuff {{
let NERDTreeBookmarksFile=expand("$HOME/.vim/NERDTreeBookmarks")
let NERDTreeIgnore=[ '\.pyc$', '\.pyo$', '\.o$', '\.git', '\.so' ]
let NERDTreeShowBookmarks=1
let NERDTreeShowFiles=1
let NERDTreeShowHidden=1
let NERDTreeQuitOnOpen=1
let NERDTreeHighlightCursorLine=1
let NERDTreeMouseMode=1
"  }}
" }}


" ***** Mode-specific settings ***** {{
" Python {{
let python_highlight_all = 1
augroup pythonSettings
  autocmd!
  autocmd FileType python syn keyword pythonDecorator True None False self is not in
  autocmd Filetype python set foldmethod=indent
augroup END
"  }}
" Ruby {{
augroup rubySettings
  autocmd!
  autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
  autocmd FileType ruby,eruby let b:match_words = 'GetRubyMatchWords()'
augroup END
"  }}
" Clojure {{
let vimclojure#HighlightBuildins = 1
let vimclojure#ParenRainbow = 0
let vimclojure#WantNailgun = 0  " Don't start the repl
let vimclojure#NailgunClient = "/usr/local/bin/ng"
augroup clojureSettings
  autocmd!
  autocmd FileType clojure set foldmarker=(,)
augroup END
"  }}
" Markdown {{
augroup markdownSettings
  autocmd!
  " Underline the current line with "=" signs
  autocmd FileType markdown map <buffer> <leader>_ yypVr=
  autocmd FileType markdown map <buffer> <leader>1 I# $ #<CR><CR><Esc>
  autocmd FileType markdown map <buffer> <leader>2 I## $ ##<CR><CR><Esc>
  autocmd FileType markdown map <buffer> <leader>3 I### $ ###<CR><CR><Esc>
  " Wrap the next word as a markdown link
  autocmd FileType markdown
        \ map <buffer> <leader>[ ysiw]ya]f]a[]<Esc>maG] jp>>A: <C-R>+<Esc>`a
  autocmd BufEnter *.md set spell
augroup END
"  }}
" }}


" Setting some colours {{
hi bufExplorerMapping guifg=white
hi Conceal guibg=black guifg=white
" }}


" Fin. {{
if filereadable(expand("~/.vimrc.local"))
  source ~/.vimrc.local
endif
" }}
