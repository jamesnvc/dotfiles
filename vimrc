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
set expandtab
set foldenable
set foldmethod=marker
set formatoptions+=n  " gq recognizes numbered lists
set gdefault  " Make substitute global by default
set hidden  " When opening a new file hide the current instead of closing it
set history=1000
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set list
set listchars=tab:▸\ ,trail:¬,extends:→,nbsp:.
set magic
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
"  }}
let g:tex_conceal="adgm"
let g:tex_flavor='latex'
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
  let l:tmp = @s
  normal ms
  keepjumps :%s/\v\s+$//e
  normal `s
  let @s = l:tmp
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
"  }}
" Normal mode bindings {{
nnoremap <leader><leader> :
nnoremap j gj
nnoremap k gk
nnoremap / /\v
nmap Y y$
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>
nmap <silent> <leader>/ :let @/=""<CR>
nmap <leader>w :w<CR>
nmap <leader>G :GitX<CR>
" Show syntax group
nmap <leader>P :call <SID>SynStack()<CR>
" Visually select the text last edited/pasted
nmap gV `[v`]
" Reflow paragraph
nmap Q gqgq
" Using this instead of autochdir
nmap <leader>cd :cd %:p:h<CR>
" NERDTree bindings
nmap <leader>n :NERDTreeClose<CR>:NERDTreeToggle<CR>
nmap <Leader>m :NERDTreeClose<CR>:NERDTreeFind<CR>
nmap <leader>N :NERDTreeClose<CR>
" Keybindings for tabs
nmap <leader>t <Esc>:tabedit .<CR>
nmap <leader>T <Esc>:tabnew<CR>
nmap gt <C-w>gf
nmap gT <C-w>gF
nmap <leader><Left> :tabprevious<CR>
nmap <leader><Right> :tabnext<CR>
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
"  }}
" }}


" ***** Miscellaneous autocmds ***** {{
if has('autocmd')
  augroup cleanUp
    " Delete trailing whitespace on save
    autocmd BufWritePre * :call CleanupWhitespace()
  augroup END
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
"  Yankring {{
let g:yankring_dot_repeat_yank = 1
"  }}
" }}


" ***** Mode-specific settings ***** {{
" Python {{
let python_highlight_all = 1
augroup pythonSettings
autocmd FileType python syn keyword pythonDecorator True None False self is not in
autocmd Filetype python set foldmethod=indent
augroup END
"  }}
" Ruby {{
augroup rubySettings
autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
augroup END
"  }}
" Clojure {{
let vimclojure#HighlightBuildins = 1
let vimclojure#ParenRainbow = 0
let vimclojure#WantNailgun = 0  " Don't start the repl
let vimclojure#NailgunClient = "/usr/local/bin/ng"
augroup clojureSettings
autocmd FileType clojure set foldmarker=(,)
augroup END
"  }}
" Markdown {{
augroup markdownSettings
" Underline the current line with "=" signs
autocmd FileType mkd map <buffer> <leader>_ yypVr=
autocmd FileType mkd map <buffer> <leader>1 I# $ #<CR><CR><Esc>
autocmd FileType mkd map <buffer> <leader>2 I## $ ##<CR><CR><Esc>
autocmd FileType mkd map <buffer> <leader>3 I### $ ###<CR><CR><Esc>
" Wrap the next word as a markdown link
autocmd FileType mkd
      \ map <buffer> <leader>[ bysw]%a[]<Esc>mao<Tab>[]: <D-v><Esc>_li
augroup END
"  }}
" }}


" Setting some colours {{
hi bufExplorerMapping guifg=white
hi Conceal guibg=black guifg=white
" }}
