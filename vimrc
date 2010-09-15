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
let mapleader = ","

colorscheme ir_black

filetype on
filetype plugin on
filetype indent on

syntax on
" }


" ***** Set stuff ***** {
set hidden  " When opening a new file hide the current instead of closing it
set ruler
set cursorline
set magic
set foldenable
set backspace=indent,eol,start
set ofu=syntaxcomplete#Complete
set completeopt=longest,menuone
set foldmethod=marker
set tabstop=2
set smarttab
set shiftwidth=2
set autoindent
set expandtab
set ignorecase
set smartcase
set showmatch
set hlsearch
set incsearch
set scrolloff=2
set wildmode=longest,list
if has('gui_running')
  set guioptions-=T
endif
set history=1000
set undolevels=1000
set pastetoggle=<F2>
set list
set visualbell
set listchars=tab:>.,trail:.,extends:#,nbsp:.
if has('autocmd')
  autocmd filetype html,xml set listchars-=tab:>.
endif
" Backup stuff {
set backup
set backupdir=$HOME/.vimbackup//
set directory=$HOME/.vimswap//
set viewdir=$HOME/.vimviews//
" Creating backup dirs if the don't exist
silent execute ' !mkdir -p $HOME/.vimbackup'
silent execute ' !mkdir -p $HOME/.vimswap'
silent execute ' !mkdir -p $HOME/.vimviews'
"  }
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
"  }
" Normal mode bindings {
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>
nnoremap ; :
nmap <silent> <leader>/ :let @/=""<CR>
nnoremap j gj
nnoremap k gk
" Reflow paragraph
nmap Q gqap
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
" Fix shift-key goofs
cmap W w
cmap WQ wq
cmap Q q
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
"  }
" Visual-mode bindings {
vmap Q gq
"  }
" Insert mode bindings {
inoremap ;; <Esc>
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
" }
