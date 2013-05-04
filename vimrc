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

if !exists('g:initially_set_colours')
  syntax enable
  set background=dark
  colorscheme Tomorrow-Night-Bright
  let g:initially_set_colours = 1
endif

" Load powerline statusbar
python from powerline.bindings.vim import source_plugin; source_plugin()

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
set cpo+=J  " make sentences have two spaces after the period
set dict=/usr/share/dict/words
set diffopt+=iwhite " Ignore trailing whitespace in diffs
set expandtab
set foldenable
set foldmethod=marker
set foldlevelstart=99
set formatoptions+=n  " gq recognizes numbered lists
set gdefault  " Make substitute global by default
set grepprg=ag\ --nogroup\ --nocolor\ --column
set hidden  " When opening a new file hide the current instead of closing it
set history=1000
set hlsearch
set ignorecase
set incsearch
set matchtime=3
set laststatus=2
set list
set listchars=tab:▸\ ,trail:¬,extends:→,precedes:←,nbsp:.
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
set showbreak=↪
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
command! -nargs=0 Restore set lines=100 columns=85
command! -nargs=0 GitX !open -a GitX %:p:h<CR>
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
nnoremap <leader>gg :grep <C-R>=expand("<cword>")<CR><CR>
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
nnoremap <leader>T :CtrlPTag<CR>
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
endif
" }}


" ***** Plugin options ***** {{
" Python-mode {{
let g:pymode_virtualenv = 1
let g:pymode_lint = 0
let g:pymode_lint_write = 0
let g:pymode_rope = 0
let g:pymode_lint_checker = 'pep8'
" }}
" Ctrl-p {{
let g:ctrlp_map = '<leader>t'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 0
let g:ctrlp_dont_split = 'NERD_tree_2'
let g:ctrp_extensions = ['tag']
let g:ctrlp_prompt_mappings = {
      \ 'PrtSelectMove("j")': ['<C-j>', '<down>', '<s-tab>'],
      \ 'PrtSelectMove("k")': ['<C-k>', '<up>', '<tab>'],
      \ 'PrtHistory(-1)': ['<C-n>'],
      \ 'PrtHistory(1)': ['<C-p>'],
      \ 'ToggleFocus()': ['<C-tab>'],
      \ }
let ctrlp_filter_greps = "".
      \ "ag -iv '\\.(" .
      \ "jar|class|swp|swo|log|so|o|pyc|jpe?g|png|gif|mo|po" .
      \ ")$' | " .
      \ "ag -v '^(\\./)?(" .
      \ "lib|classes|libs|deploy/vendor|.git|.hg|.svn|.*migrations|docs/build" .
      \ ")/'"
let my_ctrlp_user_cmd = "" .
      \ "find %s '(' -type f -o -type l ')' -maxdepth 15 " .
      \ "-not -path '*/\\.*/*' | "  .
      \ ctrlp_filter_greps
let my_ctrlp_git_cmd = "" .
      \ "cd %s && git ls-files --exclude-standard -co | " .
      \ ctrlp_filter_greps
let g:ctrlp_user_command = ['.git/', my_ctrlp_git_cmd, my_ctrlp_user_cmd]
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
" Jedi {{
let g:jedi#auto_initialization = 0
let g:jedi#goto_command = "<leader>G"
let g:jedi#use_tabs_not_buffers = 0
let g:jedi#rename_command = "<leader>R"
" }}
let g:pep8_map = '<C-8>'
let g:rails_statusline = 0
let g:indent_guides_auto_colors = 0
let g:gitgutter_enabled = 0
let g:paredit_mode = 0
" }}


" Setting some colours {{
highlight InterestingWord1 ctermbg=110
highlight InterestingWord2 ctermbg=148
highlight InterestingWord3 ctermbg=172
highlight bufexplorermapping guifg=white
highlight Conceal guibg=black guifg=white
highlight IndentGuidesOdd  guibg=red   ctermbg=DarkGray
highlight IndentGuidesEven guibg=green ctermbg=Gray
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
