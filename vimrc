set nocompatible

let mapleader = ","

if has('gui_running')
  colorscheme inkpot
else
  colorscheme darkblue
endif

" ***** Setup pathogen for loading bundles ***** 
runtime! autoload/pathogen.vim
if exists('g:loaded_pathogen')
  call pathogen#runtime_append_all_bundles()
  call pathogen#helptags()
end


" ***** Keybindings ***** 
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>
nnoremap ; :
nmap <silent> <leader>/ :let @/=""<CR>
nnoremap j gj
nnoremap k gk
vmap Q gq
nmap Q gqap
" Using this instead of autochdir
nmap <leader>cd :cd %:p:h<CR>
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
" Reopen the current file as sudo
cmap w!! w !sudo tee % > /dev/null
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
inoremap ;; <Esc>

filetype on
filetype plugin on
filetype indent on

" ***** Set stuff ***** 
set hidden  " When opening a new file hide the current instead of closing it
" if exists('+autochdir')
"   set autochdir
" endif
set backspace=indent,eol,start
set ofu=syntaxcomplete#Complete
set completeopt=longest,menuone
set spell  " Enable spell checking
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
set guioptions-=T
set history=1000
set undolevels=1000
set pastetoggle=<F2>
set list
set visualbell
set listchars=tab:>.,trail:.,extends:#,nbsp:.
if has('autocmd')
  autocmd filetype html,xml set listchars-=tab:>.
endif

" ***** Miscellaneous options *****
" NERDTree stuff
let NERDTreeBookmarksFile=expand("$HOME/.vim/NERDTreeBookmarks")
let NERDTreeIgnore=[ '\.pyc$', '\.pyo$', '\.o$', '\.git', '\.so' ]
let NERDTreeShowBookmarks=1
let NERDTreeShowFiles=1
let NERDTreeShowHidden=1
let NERDTreeQuitOnOpen=1
let NERDTreeHighlightCursorLine=1
let NERDTreeMouseMode=1
" OmniComplete stuff
if has('autocmd')
  autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
endif
