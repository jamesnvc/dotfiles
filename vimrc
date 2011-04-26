" Author: James Cash <james.cash@occasionallycogent.com>
" Source: https://github.com/jamesnvc/dotfiles


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

if !has('gui_running')
  set t_Co=256
end
set background=dark
colorscheme solarized

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
set tabstop=2
set undofile
set undolevels=1000
set virtualedit+=block
set visualbell
set wildmenu
set wildmode=list:longest,full
if has('gui_running')
  set guioptions-=T
  set guioptions-=m
  set guioptions-=rL
  set guicursor=a:blinkon0
  set guifont=Anonymous\ Pro
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
" Crazy fancy statusline {{{
" Functions {{{
" Statusline updater {{{
" Inspired by StatusLineHighlight by Ingo Karkat
function! s:StatusLine(new_stl, type, current)
let current = (a:current ? "" : "NC")
let type = a:type
let new_stl = a:new_stl

" Prepare current buffer specific text
" Syntax: <CUR> ... </CUR>
let new_stl = substitute(new_stl, '<CUR>\(.\{-,}\)</CUR>', (a:current ? '\1' : ''), 'g')

  " Prepare statusline colors
  " Syntax: #[ ... ]
  let new_stl = substitute(new_stl, '#\[\(\w\+\)\]', '%#StatusLine'.type.'\1'.current.'#', 'g')

  " Prepare statusline arrows
  " Syntax: [>] [>>] [<] [<<]
  if s:round_stl
    let new_stl = substitute(new_stl, '\[>\]',  '）', 'g')
    let new_stl = substitute(new_stl, '\[>>\]', '】', 'g')
    let new_stl = substitute(new_stl, '\[<\]',  '（', 'g')
    let new_stl = substitute(new_stl, '\[<<\]', '【', 'g')
  else
    let new_stl = substitute(new_stl, '\[>\]',  '〉', 'g')
    let new_stl = substitute(new_stl, '\[>>\]', '】', 'g')
    let new_stl = substitute(new_stl, '\[<\]',  '〈', 'g')
    let new_stl = substitute(new_stl, '\[<<\]', '【', 'g')
  endif

  if &l:statusline ==# new_stl
    " Statusline already set, nothing to do
    return
  endif

  if empty(&l:statusline)
    " No statusline is set, use my_stl
    let &l:statusline = new_stl
  else
    " Check if a custom statusline is set
    let plain_stl = substitute(&l:statusline, '%#StatusLine\w\+#', '', 'g')

    if &l:statusline ==# plain_stl
      " A custom statusline is set, don't modify
      return
    endif

    " No custom statusline is set, use my_stl
    let &l:statusline = new_stl
  endif
endfunction
" }}}
" Color dict parser {{{
function! s:StatusLineColors(colors)
for type in keys(a:colors)
  for name in keys(a:colors[type])
    let colors = {'c': a:colors[type][name][0], 'nc': a:colors[type][name][1]}
    let type = (type == 'NONE' ? '' : type)
    let name = (name == 'NONE' ? '' : name)

    if exists("colors['c'][4]")
      exec 'hi StatusLine'.type.name.' ctermbg='.colors['c'][0].' ctermfg='.colors['c'][1].' cterm='.colors['c'][2].' guibg='.colors['c'][3].' guifg='.colors['c'][4].' gui='.colors['c'][2]
    endif

    if exists("colors['nc'][4]")
      exec 'hi StatusLine'.type.name.'NC ctermbg='.colors['nc'][0].' ctermfg='.colors['nc'][1].' cterm='.colors['nc'][2].' guibg='.colors['nc'][3].' guifg='.colors['nc'][4].' gui='.colors['nc'][2]
    endif
  endfor
endfor
endfunction
" }}}
" }}}
" Default statusline {{{
let g:default_stl  = ""
let g:default_stl .= "<CUR>#[Mode] %{&paste ? 'PASTE [>] ' : ''}%{substitute(mode(), '', '^V', 'g')} #[ModeS][>>]</CUR>"
let g:default_stl .= "#[Branch] %(%{substitute(fugitive#statusline(), 'GIT(\\([a-z0-9\\-_\\./:]\\+\\))', '↱  \\1', 'gi')}#[BranchS] [>] %)" " Git branch
let g:default_stl .= "#[ModFlag]%{&readonly ? '☓ ' : ''}" " RO flag
let g:default_stl .= "#[FileName]%t " " File name
let g:default_stl .= "<CUR>#[Error]%(%{substitute(SyntasticStatuslineFlag(), '\\[Syntax: line:\\(\\d\\+\\) \\((\\(\\d\\+\\))\\)\\?\\]', '[>][>][>] SYNTAX ␤ \\1 \\2 [>][>][>]', 'i')} %)</CUR>" " Syntastic error flag
let g:default_stl .= "#[ModFlag]%(%M %)" " Modified flag
let g:default_stl .= "#[BufFlag]%(%H%W %)" " HLP,PRV flags
let g:default_stl .= "#[FileNameS][>>]" " Separator
let g:default_stl .= "#[FunctionName] " " Padding/HL group
let g:default_stl .= "%<" " Truncate right
let g:default_stl .= "<CUR>%(%{cfi#format('%s', '')} %)</CUR>" " Function name
let g:default_stl .= "%= " " Right align
let g:default_stl .= "<CUR>#[FileFormat]%{&fileformat} </CUR>" " File format
let g:default_stl .= "<CUR>#[FileEncoding]%{(&fenc == '' ? &enc : &fenc)} </CUR>" " File encoding
let g:default_stl .= "<CUR>#[Separator][<] π #[FileType]%{strlen(&ft) ? &ft : 'n/a'} </CUR>" " File type
let g:default_stl .= "#[LinePercentS][<<]#[LinePercent] %p%% " " Line/column/virtual column, Line percentage
let g:default_stl .= "#[LineNumberS][<<]#[LineNumber] ␤ %l#[LineColumn]:%c%V " " Line/column/virtual column, Line percentage
let g:default_stl .= "%{exists('g:synid') && g:synid ? '[<] '.synIDattr(synID(line('.'), col('.'), 1), 'name').' ' : ''}" " Current syntax group
" }}}
" Color dict {{{
let s:statuscolors = {
  \   'NONE': {
    \   'NONE'         : [[ 236, 231, 'bold'], [ 232, 244, 'none']]
  \ }
  \ , 'Normal': {
    \   'Mode'         : [[ 214, 235, 'bold', '#af890e', '#333333'], [                 ]]
    \ , 'ModeS'        : [[ 214, 240, 'bold', '#af890e', '#555555'], [                 ]]
    \ , 'Branch'       : [[ 240, 250, 'none', '#555555', '#777777'], [ 234, 239, 'none']]
    \ , 'BranchS'      : [[ 240, 246, 'none', '#555555', '#999999'], [ 234, 239, 'none']]
    \ , 'FileName'     : [[ 240, 231, 'bold', '#555555', '#ffffff'], [ 234, 244, 'none']]
    \ , 'FileNameS'    : [[ 240, 236, 'bold', '#555555', '#222222'], [ 234, 232, 'none']]
    \ , 'Error'        : [[ 240, 202, 'bold', '#555555', '#ff9933'], [ 234, 239, 'none']]
    \ , 'ModFlag'      : [[ 240, 196, 'bold', '#555555', '#ff0000'], [ 234, 239, 'none']]
    \ , 'BufFlag'      : [[ 240, 250, 'none', '#555555', '#bbbbbb'], [ 234, 239, 'none']]
    \ , 'FunctionName' : [[ 236, 247, 'none', '#222222', '#aaaaaa'], [ 232, 239, 'none']]
    \ , 'FileFormat'   : [[ 236, 244, 'none', '#444444', '#888888'], [ 232, 239, 'none']]
    \ , 'FileEncoding' : [[ 236, 244, 'none', '#444444', '#444444'], [ 232, 239, 'none']]
    \ , 'Separator'    : [[ 236, 242, 'none', '#444444', '#222222'], [ 232, 239, 'none']]
    \ , 'FileType'     : [[ 236, 248, 'none', '#444444', '#666666'], [ 232, 239, 'none']]
    \ , 'LinePercentS' : [[ 240, 236, 'none', '#555555', '#444444'], [ 234, 232, 'none']]
    \ , 'LinePercent'  : [[ 240, 250, 'none', '#555555', '#777777'], [ 234, 239, 'none']]
    \ , 'LineNumberS'  : [[ 252, 240, 'bold', '#cccccc', '#555555'], [ 234, 234, 'none']]
    \ , 'LineNumber'   : [[ 252, 236, 'bold', '#cccccc', '#222222'], [ 234, 244, 'none']]
    \ , 'LineColumn'   : [[ 252, 240, 'none', '#cccccc', '#555555'], [ 234, 239, 'none']]
  \ }
  \ , 'Insert': {
    \   'Mode'         : [[ 153,  23, 'bold', '#afd7ff', '#005f5f'], [                 ]]
    \ , 'ModeS'        : [[ 153,  31, 'bold', '#afd7ff', '#0087af'], [                 ]]
    \ , 'Branch'       : [[  31, 117, 'none', '#0087af', '#87d7ff'], [                 ]]
    \ , 'BranchS'      : [[  31, 117, 'none', '#0087af', '#87d7ff'], [                 ]]
    \ , 'FileName'     : [[  31, 231, 'bold', '#0087af', '#ffffff'], [                 ]]
    \ , 'FileNameS'    : [[  31,  24, 'bold', '#0087af', '#005f87'], [                 ]]
    \ , 'Error'        : [[  31, 202, 'bold', '#0087af', '#ff5f00'], [                 ]]
    \ , 'ModFlag'      : [[  31, 196, 'bold', '#0087af', '#ff0000'], [                 ]]
    \ , 'BufFlag'      : [[  31,  75, 'none', '#0087af', '#5fafff'], [                 ]]
    \ , 'FunctionName' : [[  24, 117, 'none', '#005f87', '#87d7ff'], [                 ]]
    \ , 'FileFormat'   : [[  24,  75, 'none', '#005f87', '#5fafff'], [                 ]]
    \ , 'FileEncoding' : [[  24,  75, 'none', '#005f87', '#5fafff'], [                 ]]
    \ , 'Separator'    : [[  24,  37, 'none', '#005f87', '#00afaf'], [                 ]]
    \ , 'FileType'     : [[  24,  81, 'none', '#005f87', '#5fd7ff'], [                 ]]
    \ , 'LinePercentS' : [[  31,  24, 'none', '#0087af', '#005f87'], [                 ]]
    \ , 'LinePercent'  : [[  31, 117, 'none', '#0087af', '#87d7ff'], [                 ]]
    \ , 'LineNumberS'  : [[ 117,  31, 'bold', '#87d7ff', '#0087af'], [                 ]]
    \ , 'LineNumber'   : [[ 117,  23, 'bold', '#87d7ff', '#005f5f'], [                 ]]
    \ , 'LineColumn'   : [[ 117,  31, 'none', '#87d7ff', '#0087af'], [                 ]]
  \ }
\ }
" }}}
" Set statusline {{{
augroup StatusLineHighlight
  autocmd!

  let s:round_stl = 0

  au ColorScheme * call <SID>StatusLineColors(s:statuscolors)
  au BufEnter,BufWinEnter,WinEnter,CmdwinEnter,CursorHold,BufWritePost,InsertLeave * call <SID>StatusLine((exists('b:stl') ? b:stl : g:default_stl), 'Normal', 1)
  au BufLeave,BufWinLeave,WinLeave,CmdwinLeave * call <SID>StatusLine((exists('b:stl') ? b:stl : g:default_stl), 'Normal', 0)
  au InsertEnter,CursorHoldI * call <SID>StatusLine((exists('b:stl') ? b:stl : g:default_stl), 'Insert', 1)
augroup END
" Re-set colorscheme to get autocmd going
exe 'colorscheme '.g:colors_name
" }}}
" }}}
" }}


" ***** Define commands ***** {{
command! -nargs=0 Restore set lines=100 columns=85
command! -nargs=0 GitX !open -a GitX %:p:h<CR>
command! -nargs=0 XmlIdent '[,']!xsltproc ~/.vim/misc/indent.xsl %
" Insert markdown reference-style link
function! AddMarkdownReferenceLink() " {{
  call inputsave()
  let refLink = input("Reference label: ")
  call inputrestore()
  exe "normal f]a[".refLink."]"
  let l = line(".")
  let c = col(".")
  normal Go
  " TODO: Make this cross-platform?
  .!pbpaste
  exe "normal >>I[".refLink."]: "
  call cursor(l, c)
endfunction
function! AddMarkdownReferenceLinkSel(type)
  let sel_save = &selection
  let &selection = "inclusive"

  if a:type == 'line'
    silent exe "normal '[V']S]"
  elseif a:type == 'block'
    silent exe "normal `[\<C-V>`]S]"
  else
    silent exe "normal `[v`]S]"
  endif
  let &selection = sel_save
  call AddMarkdownReferenceLink()
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
  exe "normal i\<CR>"
  exe "keepjumps :s/[\t]/" . repeat(" ", &tabstop) . "/"
  normal 0Dk$pjddk_
  let @/ = l:tmp
  let @" = l:tmp2
endfunction " }}
" Ruby Commands {{
" Ruby matching strings for matchit
function! GetRubyMatchWords()  " {{
  return '\<if>:\<end\>,\<def\>:\<end\>'
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
map <D-t> :CommandT<CR>
map <leader>x :bd!<CR>
"  }}
" Normal mode bindings {{
nnoremap <leader><leader> :
nnoremap j gj
nnoremap k gk
nnoremap / /\v
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>
nmap <silent> <leader>/ :let @/=""<CR>
nmap <leader>W :w<CR>
nmap <leader>G :Gstatus<CR>
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
inoremap <Left> <Esc>mz<<`zhi
inoremap <Right> <Esc>:exe "normal mz>>`z".(&shiftwidth+1)."l"<CR>i
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
    autocmd FileType sass SyntasticDisable
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
    autocmd BufRead,BufNewFile *.mu setfiletype mustache
    autocmd BufReadCmd *.epub call zip#Browse(expand("<amatch>"))
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
" Syntastic {{
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=2
let g:syntastic_disabled_filetypes = ['coffee']
" }}
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
  autocmd FileType markdown nmap <buffer> <leader>[ :set opfunc=AddMarkdownReferenceLinkSel<CR>g@
  autocmd FileType markdown nmap <buffer> <leader>[[ <leader>[iw
  autocmd FileType markdown vmap <buffer> <leader>[ S]:call AddMarkdownReferenceLink()<CR>
  autocmd FileType markdown imap <buffer> <C-l> <Esc>b<leader>[a
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
" vim: set foldmarker={{,}} foldlevel=0 foldmethod=marker :
" }}
