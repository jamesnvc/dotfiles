" Author: James Cash <james.cash@occasionallycogent.com>
" Source: https://github.com/jamesnvc/dotfiles


" ***** Setup pathogen for loading bundles ***** {{
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect('~/.vim/bundle')
" }}


" ***** Basic settings ***** {{
set nocompatible
set encoding=utf-8
let mapleader = ' '
let maplocalleader = '\'

if !has('gui_running')
  set t_Co=256
  let g:solarized_termcolors=16
end
syntax enable
set background=dark
let g:solarized_visibility = "low"
"colorscheme solarized
colorscheme Tomorrow-Night-Bright

" Use a bar-shaped cursor for insert mode, even through tmux.
if exists('$TMUX')
  let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
  let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"
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
set diffopt+=iwhite " Ignore trailing whitespace in diffs
set expandtab
set foldenable
set foldmethod=marker
set foldlevelstart=99
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
set makeprg=rake
set nrformats+=alpha
set omnifunc=syntaxcomplete#Complete
set pastetoggle=<F2>
if exists("&relativenumber")
  set relativenumber
endif
set ruler
set scrolloff=2
set shiftwidth=2
set softtabstop=2
set showmatch
set smartcase
set smarttab
set splitbelow
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
set wildignore+=classes " .. clojure/leiningen
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
if has('win32')
  let s:mkdirArgStr = '"\%HOME\%\.'
else
  let s:mkdirArgStr = '-p "$HOME/.'
endif
if !isdirectory(expand("~/.vimbackup"))
  silent execute ' !mkdir '.s:mkdirArgStr.'vimbackup"'
  silent execute ' !mkdir '.s:mkdirArgStr.'vimswap"'
  silent execute ' !mkdir '.s:mkdirArgStr.'vimviews"'
  silent execute ' !mkdir '.s:mkdirArgStr.'vimundo"'
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
command! -nargs=0 XmlIndent '[,']!xsltproc ~/.vim/misc/indent.xsl %
function! SearchToClipboard() " {{
  let @* = @/
endfunction
command! -nargs=0 StoC call SearchToClipboard()
" }}
" 'minimal' mode
function! MinimalMode() " {{
  highlight NonText ctermfg=white   " Match the tildes to your background
  set laststatus=0                  " No statusbar
  set nonumber                      " No line numbering
  set norelativenumber
  set showtabline=0                 " don't show the tab bar
  set foldcolumn=4                  " Add a left margin
  highlight! link FoldColumn Normal " Make it the background colour
  set wrapmargin=8                  " Add a right margin, sort of
endfunction
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
" Make a scratch buffer
function! Scratch() " {{
  split +e nofile
  set buftype=nofile bufhidden=hide
  setlocal noswapfile
endf
command! Scratch call Scratch()
" }}
" Ruby Commands {{
" Ruby matching strings for matchit
function! GetRubyMatchWords()  " {{
  return '\<if>:\<end\>,\<def\>:\<end\>,\<do\>:\<end\>'
endfunction  " }}
" Lists functions/methods in current file
function! g:ListRubyFunctions()  " {{
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
endfunction  " }}
" Searches for function/method definition under the cursor
function! g:GotoRubyFunc()  " {{
  let find_command = 'find . -type f | grep .rb  | xargs grep -n def\ '.expand('<cword>')
  echo(find_command)
  set errorformat=%f:%l:%m
  lgetexpr system(find_command)
  rightb lopen
endfunction  " }}
" Executes spec (rspec 1.3) command in different modes
" and display results in :Error buffer
" available modes:
" - file - all specs in current file
" - line - current context or current example (cursor within context {} or it   {} block
" - all - runs whole test case
function! g:RunRspec(mode)  " {{
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
endfunction  " }}

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
" Refactoring {{
function! ExtractVariable() " {{
  let name = input("Variable name: ")
  if name == ''
    return
  endif
  normal! gv
  exec "normal! c" . name
  exec "normal! O" . name . " = "
  normal! $p
endfunction
"   }}
function! InlineVariable() " {{
  let l:tmp_a = @a
  let l:tmp_b = @b
  normal! "ayiW
  " Delete variable and equal sign
  normal! 2daW
  normal! "bd$
  normal! dd
  normal! k$
  exec '/\<' . @a . '\>'
  exec ':s/\<' . @a . '\>/' . @b
  let @a = l:tmp_a
  let @b = l:tmp_b
endfunction
"   }}
"  }}
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
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
" Toggle spellchecking
noremap <leader>ss :setlocal spell!<CR>
noremap <leader>o :BufExplorer<CR>
noremap <leader>C :call HexHighlight()<CR>
noremap Y y$
" Undo tree
noremap <leader>U :GundoToggle<CR>
" Change LaTeX suite bindings from <C-j>
noremap <leader>J <Plug>IMAP_JumpForward
noremap <leader>x :bd!<CR>
noremap <leader>B :FufBuffer<CR>
noremap <leader>F :FufFileWithCurrentBufferDir<CR>
" Execute current file (assuming it's a script)
noremap <leader>R :!./%<CR>
noremap <F12> :TagbarToggle<CR>
"  }}
" Normal mode bindings {{
nnoremap <tab> %
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
nnoremap <leader>G :Gstatus<CR>
nnoremap <leader>g :grep <C-R>=expand("<cword>")<CR><CR>
" Show syntax group
nnoremap <leader>P :call <SID>SynStack()<CR>
" Visually select the text last edited/pasted
nnoremap gV `[v`]
" Reflow paragraph
nnoremap Q gqip
" Using this instead of autochdir
nnoremap <leader>cd :cd %:p:h<CR>
" NERDTree bindings
nnoremap <leader>n :NERDTreeToggle<CR>
nnoremap <Leader>m :NERDTreeClose<CR>:NERDTreeFind<CR>
nnoremap <leader>N :NERDTreeClose<CR>
nnoremap gt <C-w>gf
nnoremap gT <C-w>gF
nnoremap <Left> :tabprevious<CR>
nnoremap <Right> :tabnext<CR>
" Tabular
nnoremap <Leader>b= :Tabularize /=<CR>
nnoremap <Leader>b: :Tabularize /^[^:]*:\zs/r0c0l0<CR>
nnoremap <Leader>b, :Tabularize /^[^,]*,\zs/r0c0l0<CR>
" open URL under cursor in browser
nnoremap gb :OpenURL <cfile><CR>
nnoremap gG :OpenURL http://www.google.com/search?q=<cword><CR>
nnoremap gW :OpenURL http://en.wikipedia.org/wiki/Special:Search?search=<cword><CR>
nnoremap <leader>ri :call InlineVariable()<CR>
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
      autocmd WinLeave * setl norelativenumber
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
    autocmd BufWritePost *.py call Pyflakes()
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
  augroup END  "}}
  augroup slimv " {{
    autocmd!

    autocmd BufWinEnter            SLIMV.REPL setlocal nolist
    autocmd BufNewFile,BufReadPost SLIMV.REPL setlocal nowrap foldlevel=99
    autocmd BufNewFile,BufReadPost SLIMV.REPL nnoremap <buffer> A GA
    autocmd BufNewFile,BufReadPost SLIMV.REPL nnoremap <buffer> <localleader>R :emenu REPL.<Tab>

  augroup END "}}
endif
" }}


" ***** Plugin options ***** {{
" Python-mode {{
let g:pymode_virtualenv = 1
" }}
" Ctrl-p {{
let g:ctrlp_map = '<leader>t'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 2
" }}
" NERDTree stuff {{
let NERDTreeBookmarksFile=expand("$HOME/.vim/NERDTreeBookmarks")
let NERDTreeIgnore=[ '\.pyc$', '\.pyo$', '\.o$', '\.git', '\.so' ]
let NERDTreeShowBookmarks=1
let NERDTreeShowFiles=1
let NERDTreeShowHidden=1
let NERDTreeQuitOnOpen=1
let NERDTreeHighlightCursorLine=1
let NERDTreeMouseMode=1
let NERDTreeMinimalUI=1
let NERDTreeDirArrows=1
"  }}
" Syntastic {{
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=2
" Don't use syntastic for coffeescript (screws up) or python (pyflakes
" instead)
let g:syntastic_disabled_filetypes = ['coffee', 'python', 'sass']
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
" Slimv {{
let g:slimv_leader = '\'
let g:slimv_keybindings = 2
let g:slimv_repl_name = 'SLIMV.REPL'
let g:slimv_repl_split = 4
let g:slimv_repl_wrap = 0
let g:slimv_swank_clojure = '!dtach -n /tmp/dtach-swank.sock -r winch lein swank'
let g:paredit_mode = 0
" }}
let g:pep8_map = '<leader>8'
let g:rails_statusline = 0
let g:Powerline_symbols = 'unicode'
" }}


" Setting some colours {{
highlight InterestingWord1 ctermbg=110
highlight InterestingWord2 ctermbg=148
highlight InterestingWord3 ctermbg=172
highlight bufexplorermapping guifg=white
highlight conceal guibg=black guifg=white
if !has("gui_running")
  hi SpellBad ctermfg=Red
endif
" }}


" Fin. {{
if filereadable(expand("~/.vimrc.local"))
  source ~/.vimrc.local
endif
" vim: set foldmarker={{,}} foldlevel=0 foldmethod=marker :
" }}
