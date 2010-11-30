" Modeline and Notes {
" vim: set foldmarker={,} foldlevel=0 foldmethod=marker :
"
"   James Cash's vimrc
"
" }


" ***** Setup pathogen for loading bundles ***** {
runtime! autoload/pathogen.vim
if exists('g:loaded_pathogen')
  call pathogen#runtime_append_all_bundles()
  call pathogen#helptags()
end
" }


" ***** Basic settings ***** {
set nocompatible
set encoding=utf-8
let mapleader = " "

colorscheme ir_black

filetype on
filetype plugin on
filetype indent on

syntax on
" }


" ***** Set stuff ***** {
set autoindent
set backspace=indent,eol,start
set cole=2 " Enable 'Conceal' mode
let g:tex_conceal="adgm"
hi Conceal guibg=black guifg=white
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
if has('autocmd')
  "autocmd filetype html,xml set listchars-=tab:▸\ ,
  "autocmd FocusLost * :wa
endif
" Backup stuff {
set backup
set backupdir=$HOME/.vimbackup//
set directory=$HOME/.vimswap//
set viewdir=$HOME/.vimviews//
set undodir=$HOME/.vimundo//
" Creating backup dirs if the don't exist
silent execute ' !mkdir -p $HOME/.vimbackup'
silent execute ' !mkdir -p $HOME/.vimswap'
silent execute ' !mkdir -p $HOME/.vimviews'
silent execute ' !mkdir -p $HOME/.vimundo'
"  }
" }


" ***** Define commands ***** {
if has('gui_macvim')
  command! -nargs=0 Full set fullscreen
  command! -nargs=0 Unfull set nofullscreen
endif
command! -nargs=0 Restore set lines=100 columns=85
command! -nargs=0 GitX !open -a GitX %:p:h<CR>
" Show syntax highlighting groups for word under cursor
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
" }


" ***** Keybindings ***** {
" Normal/operator-pending/visual-mode bindings {
" Make navigating windows easier
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
" Toggle spellchecking
map <leader>ss :setlocal spell!<CR>
map <leader>o :BufExplorer<CR>
map <leader>C :call HexHighlight()<CR>
"  }
" Normal mode bindings {
nnoremap ; :
nnoremap j gj
nnoremap k gk
nnoremap / /\v
nnoremap Y y$
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>
nmap <silent> <leader>/ :let @/=""<CR>
nmap <leader>w :w<CR>
nmap <leader>G :GitX<CR>
" Show syntax group
nmap <leader>P :call <SID>SynStack()<CR>
" Bubble single lines (using "unimpared" plugin)
nmap <C-Up> [e
nmap <C-Down> ]e
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
nmap <Leader>t <Esc>:tabedit .<CR>
nmap <leader>T <Esc>:tabnew<CR>
nmap gt <C-w>gf
nmap gT <C-w>gF
nmap <leader><Left> :tabprevious<CR>
nmap <leader><Right> :tabnext<CR>
nmap <leader>1 :tabn 1<CR>
nmap <leader>2 :tabn 2<CR>
nmap <leader>3 :tabn 3<CR>
nmap <leader>4 :tabn 4<CR>
nmap <leader>5 :tabn 5<CR>
nmap <leader>6 :tabn 6<CR>
nmap <leader>7 :tabn 7<CR>
nmap <leader>8 :tabn 8<CR>
nmap <leader>9 :tabn 9<CR>
nmap <leader>10 :tabn 10<CR>
"  }
" Command-mode bindings {
" Reopen the current file as sudo
cmap w!! w !sudo tee % > /dev/null
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap s/ s/\v
"  }
" Visual-mode bindings {
vmap Q gq
" Bubble multiple lines up/down using unimpared plugin.
vmap <C-Up> [egv
vmap <C-Down> ]egv
vnoremap / /\v
"  }
" Operator-pending mode bindings {
"  }
" Insert mode bindings {
inoremap qq <Esc>
inoremap <Left> <Esc><<i
inoremap <Right> <Esc>>>i
"  }
" }


" ***** Miscellaneous autocmds ***** {
if has('autocmd')
  " Delete trailing whitespace on save
  autocmd BufWritePre * :%s/\s\+$//e
  " Warning: This enables fancy OmniCompletions for ruby, but makes loading
  " ruby files painfully slow...
  " autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
endif
" }


" ***** Plugin options ***** {
" NERDTree stuff {
let NERDTreeBookmarksFile=expand("$HOME/.vim/NERDTreeBookmarks")
let NERDTreeIgnore=[ '\.pyc$', '\.pyo$', '\.o$', '\.git', '\.so' ]
let NERDTreeShowBookmarks=1
let NERDTreeShowFiles=1
let NERDTreeShowHidden=1
let NERDTreeQuitOnOpen=1
let NERDTreeHighlightCursorLine=1
let NERDTreeMouseMode=1
"  }
"  Yankring {
let g:yankring_dot_repeat_yank = 1
"  }
" }


" ***** Mode-specific settings ***** {
" Python {
let python_highlight_all = 1
augroup pythonSettings
autocmd FileType python syn keyword pythonDecorator True None False self is not in
autocmd Filetype python set foldmethod=indent
augroup END
"  }
" Ruby {
augroup rubySettings
autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
augroup END
"  }
" Clojure {
let vimclojure#HighlightBuildins = 1
let vimclojure#ParenRainbow = 0
let vimclojure#WantNailgun = 0  " Don't start the repl
let vimclojure#NailgunClient = "/usr/local/bin/ng"
augroup clojureSettings
autocmd FileType clojure set foldmarker=(,)
augroup END
"  }
" Markdown {
" Underline the current line with "=" signs
augroup markdownSettings
autocmd FileType mkd map <buffer> <leader>_ yypVr=
autocmd FileType mkd map <buffer> <leader>H1 I# $ #<CR><CR><Esc>
autocmd FileType mkd map <buffer> <leader>H2 I## $ ##<CR><CR><Esc>
autocmd FileType mkd map <buffer> <leader>H3 I### $ ###<CR><CR><Esc>
autocmd FileType mkd
      \ map <buffer> <leader>[ bysw]%a[]<Esc>mao<Tab>[]: <D-v><Esc>_li
augroup END
"  }
" }
