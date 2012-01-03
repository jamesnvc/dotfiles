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
let g:default_stl .= "%{&ft == 'ruby' ? rvm#statusline() : ''}"
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

