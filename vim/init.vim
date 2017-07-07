" Author: James Cash <james.cash@occasionallycogent.com>
" Source: https://github.com/jamesnvc/dotfiles


" ***** Setup plug.vim for loading bundles ***** {{
call plug#begin('~/.config/nvim/plugged')
" Colorschemes
Plug 'w0ng/vim-hybrid'
Plug 'tpope/vim-vividchalk'
Plug 'morhetz/gruvbox'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Clojure
Plug 'guns/vim-sexp' | Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'tpope/vim-fireplace'
Plug 'guns/vim-clojure-static'
Plug 'guns/vim-clojure-highlight'
Plug 'clojure-vim/async-clj-omni'

" Haskell
Plug 'eagletmt/ghcmod-vim'
Plug 'eagletmt/neco-ghc'

" Rust
Plug 'racer-rust/vim-racer'
Plug 'rust-lang/rust.vim'
Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }

" Elixir
Plug 'elixir-lang/vim-elixir'
Plug 'thinca/vim-ref'
Plug 'awetzel/elixir.nvim', { 'do': 'yes \| ./install.sh' }
Plug 'w0rp/ale'

" Completion
Plug 'ujihisa/neco-look'
Plug 'Shougo/deoplete.nvim'
Plug 'Shougo/echodoc.vim'

" Denite
Plug 'Shougo/neomru.vim'
Plug 'Shougo/neoyank.vim'
Plug 'Shougo/denite.nvim'
Plug 'Shougo/vimproc.vim', {'do': 'make'}

" Tpope misc
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-afterimage'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-projectile'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-salve'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-tbone'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'

" Steve Losh misc
Plug 'sjl/threesome.vim'

" My stuff & forks
Plug 'jamesnvc/potion'
Plug 'jamesnvc/vim-penyocomic'

" misc
Plug 'Konfekt/FastFold'
Plug 'Rip-Rip/clang_complete'
Plug 'airblade/vim-gitgutter'
"Plug 'benekastah/neomake'
Plug 'godlygeek/tabular'
Plug 'gregsexton/gitv'
Plug 'groenewege/vim-less'
Plug 'juvenn/mustache.vim'
Plug 'kballard/vim-swift'
Plug 'kchmck/vim-coffee-script'
Plug 'klen/python-mode'
Plug 'majutsushi/tagbar'
Plug 'maksimr/vim-jsbeautify'
Plug 'michaeljsmith/vim-indent-object'
Plug 'msanders/cocoa.vim'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'nelstrom/vim-markdown-folding'
Plug 'neovim/node-host'
Plug 'othree/html5.vim'
Plug 'pangloss/vim-javascript'
Plug 'scrooloose/nerdcommenter'
"Plug 'vim-syntastic/syntastic'
Plug 'tikhomirov/vim-glsl'
Plug 'timrobinson/fsharp-vim'
Plug 'wlangstroth/vim-racket'
Plug 'andreimaxim/vim-io'
Plug 'raichoo/purescript-vim'

" neovim plugins in CL
" Disabling for now, since it makes startup time v. long
" also crashes :UpdateRemovePlugins; reinvestigate later?
"Plug 'adolenc/cl-neovim'

call plug#end()

call plug#helptags()
" }}


" ***** Neovim stuff ***** {{
if has('nvim')
  let g:python_host_prog = expand('/usr/bin/python2')
  let g:python3_host_prog = expand('~/.pyenv/shims/python3')
  set termguicolors
  " TODO: make conditional
  "let g:python_host_prog = '/Users/james/.pythonbrew/pythons/Python-2.7.2/bin/python'
  "let g:python3_host_prog = '/usr/local/var/pyenv/shims/python'
endif
" }}


" ***** Basic settings ***** {{
"set encoding=utf-8
let mapleader = ' '
let maplocalleader = '\'


if !exists('g:initially_set_colours')
  syntax enable
  set background=dark
  let g:gruvbox_italic = 1
  let g:gruvbox_contrast_dark = 'hard'
  colorscheme gruvbox
  let g:initially_set_colours = 1
endif

filetype on
filetype plugin on
filetype indent on
" }}


" ***** Set stuff ***** {{
set autoindent
set backspace=indent,eol,start
if exists("&conceallevel")
  set conceallevel=2 " Enable 'Conceal' mode
endif
set completeopt=longest,menuone,preview
set cursorline
if exists("&cryptmethod")
  set cryptmethod=blowfish
endif
set cpo+=J  " make sentences have two spaces after the period
set dict=/usr/share/dict/words
set diffopt+=iwhite " Ignore trailing whitespace in diffs
set expandtab
set foldenable
set foldmethod=marker
set foldlevelstart=99
set formatoptions+=n  " gq recognizes numbered lists
set gdefault  " Make substitute global by default
set grepprg=rg\ --vimgrep
set hidden  " When opening a new file hide the current instead of closing it
set history=1000
set hlsearch
set ignorecase
set incsearch
set inccommand=nosplit " when doing a substitute, show the results live
set matchtime=3
set laststatus=2
set lazyredraw " redraw only when required (speed up macros)
set list
set listchars=tab:▸\ ,trail:¬,extends:→,precedes:←,nbsp:.
set magic
set makeprg=rake
set nrformats+=alpha
set omnifunc=syntaxcomplete#Complete
set pastetoggle=<F2>
set path-=/usr/include
set path+=**
if exists("&relativenumber")
  set relativenumber
endif
set ruler
set scrolloff=2
set shiftwidth=2
set softtabstop=2
set showbreak=↪
set noshowmode
set showmatch
set smartcase
set smarttab
set splitright
set tabstop=2
if exists("&undofile")
  set undofile
endif
set undolevels=1000
set virtualedit+=block
set visualbell
set wildmenu
set wildmode=list:longest,full
set wildignore+=.hg,.git,.svn " Ignore version control files...
set wildignore+=*.aux,*.out,*.toc " ...LaTeX chaff
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifst " ...compiled binary files
set wildignore+=*.DS_Store " ...OS X weird thing
set wildignore+=*.pyc " ...python bytecode
if has('win32')
  set shellslash
endif
" Backup stuff {{
set backup
set backupdir=$HOME/.vimbackup//
set directory=$HOME/.vimswap//
set viewdir=$HOME/.vimviews//
if exists("&undodir")
  set undodir=$HOME/.vimundo//
endif
" Creating backup dirs if they don't exist
for dir in [&backupdir, &directory, &undodir, &directory]
  if !isdirectory(expand(dir))
    call mkdir(expand(dir), "p")
  endif
endfor
"  }}
let g:tex_conceal="adgm"
let g:tex_flavor='latex'
let g:indent_guides_start_level=2
let g:indent_guides_guide_size=1
" }}


" ***** Define commands ***** {{
command! -nargs=0 XmlIndent '[,']!xsltproc ~/.vim/misc/indent.xsl %
" Pulse line {{{
function! s:Pulse() " {{{
  let current_window = winnr()
  windo set nocursorline
  execute current_window . 'wincmd w'
  setlocal cursorline

  redir => old_hi
    silent execute 'hi CursorLine'
  redir END
  let old_hi = split(old_hi, '\n')[0]
  let old_hi = substitute(old_hi, 'xxx', '', '')

  let steps = 9
  let width = 1
  let start = width
  let end = steps * width
  let color = 233
  for i in range(start, end, width)
    execute "hi CursorLine ctermbg=" . (color + i)
    redraw
    sleep 6m
  endfor
  for i in range(end, start, -1 * width)
    execute "hi CursorLine ctermbg=" . (color + i)
    redraw
    sleep 6m
  endfor

  execute 'hi ' . old_hi
endfunction " }}}
command! -nargs=0 Pulse call s:Pulse()
" }}}
function! SearchToClipboard() " {{
  let @* = @/
endfunction
command! -nargs=0 StoC call SearchToClipboard()
" }}
" Show syntax highlighting groups for word under cursor
function! <SID>SynStack() " {{
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc "}}
" Delete trailing whitespace
function! CleanupWhitespace() " {{
  let _s = @/
  let l  = line(".")
  let c  = col(".")
  keepjumps :%s/\v\s+$//e
  let @/ = _s
  call cursor(l, c)
endfunction "}}
" Redirect a command to the clipboard
function! RedirToClipboardFunction(cmd, ...) " {{
  let cmd = a:cmd . " " . join(a:000, " ")
  redir @*>
  exe cmd
  redir END
endfunction
command! -complete=command -nargs=+ RedirToClipboard
      \ silent! call RedirToClipboardFunction(<f-args>)
" }}
" Run jslint on the current file
function! JSLintFile() " {{
  if !executable("jsl")
    echo "jsl not installed"
    return
  end
  let lint_cmd = "jsl -conf \"" . expand("~") . "/.jsl.conf\""
        \ . " -nologo -nofilelisting -nosummary -process \"" .
        \ expand("%") . "\""
  cexpr system(lint_cmd)
endfunction
command! -nargs=0 JSLint call JSLintFile()
" }}
" Turn tabs into spaces from the cursor to indent.
function! TabsToSpaces() " {{
  let l:tmp  = @/
  let l:tmp2 = @"
  " Using exe so we can insert the newline character
  exe "normal! i\<CR>"
  exe "keepjumps :s/[\t]/" . repeat(" ", &tabstop) . "/"
  normal! 0Dk$pjddk_
  let @/ = l:tmp
  let @" = l:tmp2
endfunction " }}
" Folding text better
function! MyFoldText() " {{
  let line = getline(v:foldstart)
  let nucolwidth = &foldcolumn + ( &number || &relativenumber ) * &numberwidth
  let windowwidth = winwidth(0) - nucolwidth - 3
  let foldedlinecount = v:foldend - v:foldstart

  " Expand tabs to spaces
  let onetab = strpart('         ', 0, &tabstop)
  let line = substitute(line, '\t', onetab, 'g')

  let line = strpart(line, 0, windowwidth - 2 - len(foldedlinecount))
  let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
  return line . '…' . repeat(" ", fillcharcount) . foldedlinecount . '…' . ' '
endf " }}
set foldtext=MyFoldText()
" Make a scratch buffer
function! Scratch() " {{
  split +e nofile
  set buftype=nofile bufhidden=hide
  setlocal noswapfile
endf
command! Scratch call Scratch()
" }}
" Tab setting functions {{
" From Wolever
" HardTabs([width=8]): Sets up the current buffers so that:
" - '\t' is 'widith' wide
" - '<tab>' inserts a '\t'
" - '>>' shifts one tab
function! HardTabs(...) " {{
    let width = (a:0 > 0? a:1 : 8)
    let &l:tabstop=width
    let &l:softtabstop=width
    let &l:shiftwidth=width
    let &l:expandtab=0
  endfunction " }}

" SoftTabs([softWidth=4, [hardWidth=8]]): Sets up the current buffers so that:
" - '\t' is 'hardWidth' wide
" - '<tab>' inserts 'softWidth' spaces
" - '>>' shifts with 'softWidth' spaces
fun! SoftTabs(...) " {{
    let softWidth = (a:0 > 0? a:1 : 4)
    let hardWidth = (a:0 > 1? a:2 : 8)
    let &l:tabstop=hardWidth
    let &l:softtabstop=softWidth
    let &l:shiftwidth=softWidth
    let &l:expandtab=1
endfun " }}
"  }}
" Convert quickfix/vimgrep list to args {{
command! -nargs=0 -bar Qargs execute 'args' QuickfixFilenames()

" populate the argument list with each of the files named in the quickfix list
function! QuickfixFilenames()
  let buffer_numbers = {}
  for quickfix_item in getqflist()
    let buffer_numbers[quickfix_item['bufnr']] = bufname(quickfix_item['bufnr'])
  endfor
  return join(map(values(buffer_numbers), 'fnameescape(v:val)'))
endfunction
" }}
" }}


" ***** Keybindings ***** {{
" Normal/operator-pending/visual-mode bindings {{
noremap * :let @/="\\<<C-r><C-w>\\>"<CR>
noremap j gj
noremap k gk
noremap gj j
noremap gk k
" Make navigating windows easier
noremap <C-h> <C-w>h
" for neovim
noremap <BS> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
" Toggle spellchecking
noremap <leader>ss :setlocal spell!<CR>
noremap Y y$
" Undo tree
noremap <leader>x :bd!<CR>
" Execute current file (assuming it's a script)
noremap <leader>R :!./%<CR>
noremap <F12> :TagbarToggle<CR>
"  }}
" Normal mode bindings {{
nnoremap <leader><leader> :
nnoremap / /\v
nnoremap <silent> <leader>* :exe 'vimgrep /'.@/.'/g %'<CR>:copen<CR>
nnoremap <silent> <leader>ev :e $MYVIMRC<CR>
if has("gui_running")
  nnoremap <silent> <leader>sv :so $MYVIMRC<CR>:so $MYGVIMRC<CR>
else
  nnoremap <silent> <leader>sv :so $MYVIMRC<CR>
endif
" Highlight text under cursor {{
nnoremap <silent> <leader>hh :execute 'match InterestingWord1 /\<<C-r><C-w>\>/'<CR>
nnoremap <silent> <leader>h1 :execute 'match InterestingWord1 /\<<C-r><C-w>\>/'<CR>
nnoremap <silent> <leader>h2 :execute '2match InterestingWord2 /\<<C-r><C-w>\>/'<CR>
nnoremap <silent> <leader>h3 :execute '3match InterestingWord3 /\<<C-r><C-w>\>/'<CR>
" }}
nnoremap <silent> <leader>/ :let @/=""<CR>
nnoremap <leader>W :w<CR>
nnoremap <leader>z zMzv
" Fugitive bindings {{
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gw :Gwrite<CR>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gco :Gcheckout<CR>
nnoremap <leader>gci :Gcommit<CR>
nnoremap <leader>gm :Gmove<CR>
nnoremap <leader>gr :Gremove<CR>
" }}
" Show syntax group
nnoremap <leader>P :call <SID>SynStack()<CR>
" Visually select the text last edited/pasted
nnoremap gV `[v`]
" Reflow paragraph
nnoremap Q gqip
" Using this instead of autochdir
nnoremap <leader>cd :cd %:p:h<CR>
nnoremap gt <C-w>gf
nnoremap gT <C-w>gF
nnoremap <Left> :tabprevious<CR>
nnoremap <Right> :tabnext<CR>
" Tabular
nnoremap <Leader>b= :Tabularize /=<CR>
nnoremap <Leader>b: :Tabularize /^[^:]*:\zs/r0c0l0<CR>
nnoremap <Leader>b, :Tabularize /^[^,]*,\zs/r0c0l0<CR>
" Denite
nnoremap <leader>t :<C-u>Denite -buffer-name=files
      \ `finddir('.git', ';') != '' ? 'file_rec/git' : 'file_rec'`<CR>
nnoremap <leader>o :<C-u>Denite -buffer-name=buffers buffer<CR>
nnoremap <leader>l :<C-u>Denite -buffer-name=lines line<CR>
" Map <leader>n to move to nth split
for n in range(1, 9)
  exe "nnoremap <silent> <leader>" . n . " :" . n . "wincmd w<CR>"
endfor
" Focus the current line (overwrites C-z, use :sus te suspend), wipes f
" register
nnoremap <C-z> mfzMzvzz`f:Pulse<CR>
"  }}
" Command-mode bindings {{
" Reopen the current file as sudo
cnoremap w!! w !sudo tee % > /dev/null
" Expand to the directory of the current file
cnoremap %% <C-R>=expand('%:h').'/'<cr>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap s/ s/\v
cnoremap e%% echo expand("%:p")<cr>
"  }}
" Visual-mode bindings {{
vnoremap <leader><leader> :
vnoremap <tab> %
vnoremap Q gq
" Bubble multiple lines up/down using unimpared plugin.
vnoremap <C-Up> [egv
vnoremap <C-Down> ]egv
vnoremap / /\v
" Tabular
vnoremap <Leader>b= :Tabularize /=<CR>
vnoremap <Leader>b: :Tabularize /:\zs<CR>
vnoremap <leader>rv :call ExtractVariable()<CR>
" Fix linewise visual selection of various text objects
nnoremap Vit vitVkoj
nnoremap Vat vatV
nnoremap Vab vabV
nnoremap VaB vaBV
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
inoremap <C-Space> <C-X><C-O>
inoremap <Left> <C-d>
inoremap <Right> <C-t>
inoremap <C-a> <Esc>I
inoremap <C-e> <Esc>A
" Make end of sentences set an undo point to facilitate typing long stretches
inoremap . .<C-g>u
inoremap ! !<C-g>u
inoremap ? ?<C-g>u
inoremap : :<C-g>u
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
  augroup filetypes  " {{
    autocmd!
    autocmd BufReadCmd *.epub call zip#Browse(expand("<amatch>"))
    autocmd BufEnter *.md setl makeprg=rake
  augroup END  "}}
  augroup completion  " {{
    autocmd!
    autocmd FileType python setl omnifunc=pythoncomplete#Complete
    autocmd FileType javascript setl omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType html setl omnifunc=htmlcomplete#CompleteTags
    autocmd FileType css setl omnifunc=csscomplete#CompleteCSS
    autocmd FileType vimwiki setl completefunc=googlescribe#Complete
    autocmd FileType markdown setl completefunc=googlescribe#Complete
    autocmd FileType gitcommit setl completefunc=googlescribe#Complete
    autocmd FileType ruby setl omnifunc=rubycomplete#Complete
  augroup END  "}}
  augroup fugitive  " {{
    autocmd!
    autocmd User fugitive
      \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
      \   nnoremap <buffer> .. :edit %:h<CR> |
      \ endif
    autocmd BufReadPost fugitive://* set bufhidden=delete
    autocmd BufNewFile,BufRead .git/index setlocal nolist
  augroup END  "}}
  augroup nonumberbuffers " {{
    autocmd BufEnter term://* setlocal nonumber norelativenumber
    autocmd BufEnter man://* setlocal nonumber norelativenumber
  augroup END "}}
  augroup deoplete_stuff "{{
    autocmd BufEnter *.cljs let b:deoplete_disable_auto_complete = 1
  augroup END "}}
endif
" }}


" ***** Plugin options ***** {{
" Denite {{
" Change mappings.
call denite#custom#map('insert', '<C-n>', '<denite:move_to_next_line>', 'noremap')
call denite#custom#map('insert', '<C-p>', '<denite:move_to_previous_line>', 'noremap')
call denite#custom#map('insert', '<C-s>', '<denite:do_action:split>', 'noremap')
call denite#custom#map('insert', '<C-v>', '<denite:do_action:vsplit>', 'noremap')
call denite#custom#map('insert', '<C-l>', '<denite:redraw>', 'noremap')
call denite#custom#map('normal', '<C-l>', '<denite:redraw>', 'noremap')
" Define alias
call denite#custom#alias('source', 'file_rec/git', 'file_rec')
call denite#custom#var('file_rec/git', 'command',
      \ ['git', 'ls-files', '-co', '--exclude-standard'])
" }}
" Syntastic {{
let g:syntastic_enable_signs = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 2
" Don't use syntastic for coffeescript (screws up) or python (pyflakes
" instead)
let g:syntastic_disabled_filetypes = ['coffee', 'python', 'sass']
if executable('proselint')
  let g:syntastic_markdown_checkers = ['proselint']
endif
if executable('ocamlmerlin') && has('python')
  let s:ocamlmerlin = substitute(system('opam config var share'), '\n$', '', '''') . "/ocamlmerlin"
  execute "set rtp+=".s:ocamlmerlin."/vim"
  let g:syntastic_ocaml_checkers = ['merlin']
endif
if executable('joker')
  let g:syntastic_clojure_checkers = ['joker']
  let g:syntastic_clojure_joker_args = ['--lint']
  augroup clojureLinter
    autocmd BufEnter *.cljs let b:syntastic_clojure_joker_args = ['--lintcljs']
  augroup END
  augroup ednClinter
    autocmd BufEnter *.edn let b:syntastic_clojure_joker_args = ['--lintedn']
  augroup END
endif
" }}
" UltiSnips  {{
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
" }}
" Tagbar {{
let g:tagbar_ctags_bin = '/usr/local/bin/ctags'
let g:tagbar_usearrows = 1
" }}
" deoplete {{
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1
let g:deoplete#keyword_patterns = {}
let g:deoplete#keyword_patterns.clojure = '[\w!$%&*+/:<=>?@\^_~\-\.#]*'
let g:deoplete#omni#input_patterns = {}
let g:deoplete#omni#input_patterns.c = '[^.\d *\t]\%(\.\|->\)\w*'
" TODO: make this not match inside string or comment
let g:deoplete#omni#input_patterns.clojure = '[^\d() \t]+'
let g:deoplete#omni#input_patterns.cpp = '[^.\d *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
let g:deoplete#omni#input_patterns.css   = '^\s\+\w\+\|\w\+[):;]\?\s\+\w*\|[@!]'
let g:deoplete#omni#input_patterns.go = '[^.\d *\t]\.\w*'
let g:deoplete#omni#input_patterns.html = '<[^>]*'
let g:deoplete#omni#input_patterns.javascript = '[^. \t]\.\%(\h\w*\)\?'
let g:deoplete#omni#input_patterns.md   = '<[^>]*'
" TODO: fix python complete
"let g:deoplete#omni#input_patterns.python = '[^. *\t]\.\h\w*\'
"let g:deoplete#omni#input_patterns.python3 = '[^. *\t]\.\h\w*\'
let g:deoplete#omni#input_patterns.ruby = ['[^. *\t]\.\w*', '\h\w*::']
let g:deoplete#omni#input_patterns.sass   = '^\s\+\w\+\|\w\+[):;]\?\s\+\w*\|[@!]'
let g:deoplete#omni#input_patterns.scss   = '^\s\+\w\+\|\w\+[):;]\?\s\+\w*\|[@!]'
let g:deoplete#omni#input_patterns.xml  = '<[^>]*'
autocmd CmdwinEnter * let b:deoplete_sources = ['buffer']
let g:deoplete#sources = {}
let g:deoplete#sources._ = ['buffer', 'look']
let g:deoplete#sources.clojure = ['buffer', 'omni', 'look']
let g:deoplete#sources.rust = ['buffer', 'racer']
let g:deoplete#sources.haskell = ['buffer', 'ghc']
" }}
" airline {{
let g:airline_powerline_fonts = 1
let g:airline_theme = 'gruvbox'
" }}
" rust stuff {{
let g:racer_cmd = expand("~/.cargo/bin/racer")
let g:rustfmt_commond=expand("~/.cargo/bin/rustfmt")
let $RUST_SRC_PATH = expand("~/.multirust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
let $CARGO_HOME = expand("~/.cargo")
" }}
" language client {{
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ }
" }}
let g:LanguageClient_autoStart = 1
let g:haskellmode_completion_ghc = 0
let g:clang_library_path = '/usr/lib/llvm-3.8/lib/libclang.so.1'
let g:pymode_rope_completion = 0
let g:pymode_rope_complete_on_dot = 0
let g:echodoc_enable_at_startup = 1
let g:indent_guides_auto_colors = 0
let g:gitgutter_enabled = 1
let g:haddock_browser= 'open'
let g:clojure_fuzzy_indent_patterns = ['^with', '^def', '^let', '^clone-for',
      \ '^complex', '^match', '^POST', '^GET', '^DELETE', '^PUT', '^context',
      \ '^OPTIONS']
let g:clojure_special_indent_words = 'deftype,defrecord,reify,proxy,extend-type,extend-protocol,letfn,defcomponent,defcomponentmethod,defui'
let g:projectiles = {
      \   "project.clj": {
      \     "src/*.clj": {
      \       "command": "src",
      \       "template": ["(ns %d)"],
      \       "alternate": "test/%s_test.clj",
      \     },
      \     "test/*_test.clj": {
      \       "command": "test",
      \       "template": ["(ns %d-test",
      \                    "  (:require [clojure.test :refer :all]",
      \                    "            %d))"],
      \       "alternate": "src/%s.clj",
      \     }
      \   }
      \ }
" }}


" Setting some colours {{
highlight InterestingWord1 ctermbg=110
highlight InterestingWord2 ctermbg=148
highlight InterestingWord3 ctermbg=172
highlight bufexplorermapping guifg=white
highlight IndentGuidesOdd  guibg=red   ctermbg=DarkGray
highlight IndentGuidesEven guibg=green ctermbg=Gray
highlight SpellBad guifg=Red
"highlight Comment cterm=Italic
" }}


" Fin. {{
if filereadable(expand("~/.vimrc.local"))
  source ~/.vimrc.local
endif
" vim: set foldmarker={{,}} foldlevel=0 foldmethod=marker :
" }}
