syntax enable
filetype on
filetype plugin on
filetype indent on

let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
let mapleader = ' '
let maplocalleader = '\'

" Set stuff
set background=dark
set encoding=utf-8
set expandtab
set ignorecase
set smartcase
set hlsearch
set relativenumber
set scrolloff=2
set shiftwidth=2
set softtabstop=2
set tabstop=2
set showbreak=↪
set list
set listchars=tab:▸\ ,trail:¬,extends:→,precedes:←,nbsp:.
set grepprg=ag\ --nogroup\ --nocolor\ --column
set hidden  " When opening a new file hide the current instead of closing it
set virtualedit+=block
set visualbell
set undofile
set undolevels=1000
set wildmenu
set wildmode=list:longest,full
set wildignore+=.hg,.git,.svn " Ignore version control files...
set wildignore+=*.aux,*.out,*.toc " ...LaTeX chaff
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifst " ...compiled binary files
set wildignore+=*.DS_Store " ...OS X weird thing
set wildignore+=*.pyc " ...python bytecode
set backup
set backupdir=$HOME/.vimbackup//
set directory=$HOME/.vimswap//
set viewdir=$HOME/.vimviews//
set undodir=$HOME/.vimundo//
" Creating backup dirs if they don't exist
for dir in [&backupdir, &directory, &undodir, &directory]
  if !isdirectory(expand(dir))
    call mkdir(expand(dir), "p")
  endif
endfor

" Keybindings
" move screen-linewise by default
noremap j gj
noremap k gk
noremap gj j
noremap gk k
" window navigation
noremap <C-h> <C-w>h
noremap <BS> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
" Y acts consistently
noremap Y y$
" dispose of buffers quickly
noremap <leader>x :bd!<CR>
" go to command shortcut
nnoremap <leader><leader> :
vnoremap <leader><leader> :
" always use very-magic flag when searching
nnoremap / /\v
" quick reloading of nvimrc
nnoremap <silent> <leader>ev :e $MYVIMRC<CR>
nnoremap <silent> <leader>sv :so $MYVIMRC<CR>
" save a lot
nnoremap <leader>W :w<CR>
" Fugitive bindings
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gw :Gwrite<CR>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gco :Gcheckout<CR>
nnoremap <leader>gci :Gcommit<CR>
nnoremap <leader>gm :Gmove<CR>
nnoremap <leader>gr :Gremove<CR>
" tab navigation
nnoremap <Left> :tabprevious<CR>
nnoremap <Right> :tabnext<CR>
" clear search highlight
nnoremap <leader>/ :let @/=""<CR>
" Make end of sentences set an undo point to facilitate typing long stretches
inoremap . .<C-g>u
inoremap ! !<C-g>u
inoremap ? ?<C-g>u
inoremap : :<C-g>u
" insert mode editing shortcuts
inoremap <Left> <C-d>
inoremap <Right> <C-t>
inoremap <C-a> <Esc>I
inoremap <C-e> <Esc>A

" Autocommands
augroup misc  " {{
  autocmd!
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
  " Onyl show line numbers in current window
  if exists("&relativenumber")
    autocmd WinEnter * setl relativenumber
    autocmd WinLeave * setl number
  else
    autocmd WinEnter * setl number
    autocmd WinLeave * setl nonumber
  endif
  autocmd VimResized * :wincmd =
augroup END  " }}

" Plugins
call plug#begin('~/.nvim/plugged')
" My stuff
Plug 'jamesnvc/vim-tomorrow-theme'
" Misc
Plug 'guns/vim-sexp'
Plug 'NLKNguyen/papercolor-theme'
" Tpope
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-tbone'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
call plug#end()
