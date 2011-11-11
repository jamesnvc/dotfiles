" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/AnsiEscPlugin.vim	[[[1
30
" AnsiEscPlugin.vim
"   Author: Charles E. Campbell, Jr.
"   Date:   Apr 07, 2010
"   Version: 12
" ---------------------------------------------------------------------
"  Load Once: {{{1
if &cp || exists("g:loaded_AnsiEscPlugin")
 finish
endif
let g:loaded_AnsiEscPlugin = "v12"
let s:keepcpo              = &cpo
set cpo&vim

" ---------------------------------------------------------------------
"  Public Interface: {{{1
com! -bang -nargs=0 AnsiEsc	:call AnsiEsc#AnsiEsc(<bang>0)

" DrChip Menu Support: {{{2
if has("gui_running") && has("menu") && &go =~ 'm'
 if !exists("g:DrChipTopLvlMenu")
  let g:DrChipTopLvlMenu= "DrChip."
 endif
 exe 'menu '.g:DrChipTopLvlMenu.'AnsiEsc.Start<tab>:AnsiEsc		:AnsiEsc<cr>'
endif

" ---------------------------------------------------------------------
"  Restore: {{{1
let &cpo= s:keepcpo
unlet s:keepcpo
" vim: ts=4 fdm=marker
autoload/AnsiEsc.vim	[[[1
848
" AnsiEsc.vim: Uses syntax highlighting.  A vim 7.0 plugin!
" Language:		Text with ansi escape sequences
" Maintainer:	Charles E. Campbell <NdrOchipS@PcampbellAfamily.Mbiz>
" Version:		12
" Date:		Dec 13, 2010
"
" Usage: :AnsiEsc
"
"   Note: this plugin requires Vince Negri's conceal-ownsyntax patch
"         See http://groups.google.com/group/vim_dev/web/vim-patches, Patch#14
"
" GetLatestVimScripts: 302 1 :AutoInstall: AnsiEsc.vim
"redraw!|call DechoSep()|call inputsave()|call input("Press <cr> to continue")|call inputrestore()
" ---------------------------------------------------------------------
"DechoTabOn
"  Load Once: {{{1
if exists("g:loaded_AnsiEsc")
 finish
endif
let g:loaded_AnsiEsc = "v12"
if v:version < 700
 echohl WarningMsg
 echo "***warning*** this version of AnsiEsc needs vim 7.0"
 echohl Normal
 finish
endif
let s:keepcpo= &cpo
set cpo&vim

" ---------------------------------------------------------------------
" AnsiEsc#AnsiEsc: toggles ansi-escape code visualization {{{2
fun! AnsiEsc#AnsiEsc(rebuild)
"  call Dfunc("AnsiEsc#AnsiEsc(rebuild=".a:rebuild.")")
  if a:rebuild
"   call Decho("rebuilding AnsiEsc tables")
   call AnsiEsc#AnsiEsc(0)
   call AnsiEsc#AnsiEsc(0)
"   call Dret("AnsiEsc#AnsiEsc")
   return
  endif
  let bn= bufnr("%")
  if !exists("s:AnsiEsc_enabled_{bn}")
   let s:AnsiEsc_enabled_{bn}= 0
  endif
  if s:AnsiEsc_enabled_{bn}
   " disable AnsiEsc highlighting
"   call Decho("disable AnsiEsc highlighting: s:AnsiEsc_ft_".bn."<".s:AnsiEsc_ft_{bn}."> bn#".bn)
   if exists("g:colors_name")|let colorname= g:colors_name|endif
   if exists("s:conckeep_{bufnr('%')}")|let &l:conc= s:conckeep_{bufnr('%')}|unlet s:conckeep_{bufnr('%')}|endif
   if exists("s:colekeep_{bufnr('%')}")|let &l:cole= s:colekeep_{bufnr('%')}|unlet s:colekeep_{bufnr('%')}|endif
   if exists("s:cocukeep_{bufnr('%')}")|let &l:cocu= s:cocukeep_{bufnr('%')}|unlet s:cocukeep_{bufnr('%')}|endif
   hi! link ansiStop NONE
   syn clear
   hi  clear
   syn reset
   exe "set ft=".s:AnsiEsc_ft_{bn}
   if exists("colorname")|exe "colors ".colorname|endif
   let s:AnsiEsc_enabled_{bn}= 0
   if has("gui_running") && has("menu") && &go =~ 'm'
    " menu support
    exe 'silent! unmenu '.g:DrChipTopLvlMenu.'AnsiEsc'
    exe 'menu '.g:DrChipTopLvlMenu.'AnsiEsc.Start<tab>:AnsiEsc		:AnsiEsc<cr>'
   endif
   let &l:hl= s:hlkeep_{bufnr("%")}
"   call Dret("AnsiEsc#AnsiEsc")
   return
  else
   let s:AnsiEsc_ft_{bn}      = &ft
   let s:AnsiEsc_enabled_{bn} = 1
"   call Decho("enable AnsiEsc highlighting: s:AnsiEsc_ft_".bn."<".s:AnsiEsc_ft_{bn}."> bn#".bn)
   if has("gui_running") && has("menu") && &go =~ 'm'
    " menu support
    exe 'silent! unmenu '.g:DrChipTopLvlMenu.'AnsiEsc'
    exe 'menu '.g:DrChipTopLvlMenu.'AnsiEsc.Stop<tab>:AnsiEsc		:AnsiEsc<cr>'
   endif

   " -----------------
   "  Conceal Support: {{{2
   " -----------------
   if has("conceal")
    if v:version < 703
     if &l:conc != 3
      let s:conckeep_{bufnr('%')}= &cole
      setlocal conc=3
"      call Decho("l:conc=".&l:conc)
     endif
    else
     if &l:cole != 3 || &l:cocu != "nv"
      let s:colekeep_{bufnr('%')}= &l:cole
      let s:cocukeep_{bufnr('%')}= &l:cocu
      setlocal cole=3 cocu=nv
"      call Decho("l:cole=".&l:cole." l:cocu=".&l:cocu)
     endif
    endif
   endif
  endif

  syn clear

  " suppress escaped sequences that don't involve colors (which may or may not be ansi-compliant)
  if has("conceal")
   syn match ansiSuppress	conceal	'\e\[[0-9;]*[^m]'
   syn match ansiSuppress	conceal	'\e\[?\d*[^m]'
   syn match ansiSuppress	conceal	'\b'
  else
   syn match ansiSuppress		'\e\[[0-9;]*[^m]'
   syn match ansiSuppress	conceal	'\e\[?\d*[^m]'
   syn match ansiSuppress		'\b'
  endif

  " ------------------------------
  " Ansi Escape Sequence Handling: {{{2
  " ------------------------------
  syn region ansiNone		start="\e\[[01;]m"  end="\e\["me=e-2 contains=ansiConceal
  syn region ansiNone		start="\e\[m"       end="\e\["me=e-2 contains=ansiConceal

  syn region ansiBlack		start="\e\[;\=0\{0,2};\=30m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRed		start="\e\[;\=0\{0,2};\=31m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiGreen		start="\e\[;\=0\{0,2};\=32m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiYellow		start="\e\[;\=0\{0,2};\=33m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlue		start="\e\[;\=0\{0,2};\=34m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiMagenta	start="\e\[;\=0\{0,2};\=35m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiCyan		start="\e\[;\=0\{0,2};\=36m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiWhite		start="\e\[;\=0\{0,2};\=37m" end="\e\["me=e-2 contains=ansiConceal

  syn region ansiBlackBg	start="\e\[;\=0\{0,2};\=\%(1;\)\=40\%(1;\)\=m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRedBg		start="\e\[;\=0\{0,2};\=\%(1;\)\=41\%(1;\)\=m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiGreenBg	start="\e\[;\=0\{0,2};\=\%(1;\)\=42\%(1;\)\=m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiYellowBg	start="\e\[;\=0\{0,2};\=\%(1;\)\=43\%(1;\)\=m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlueBg		start="\e\[;\=0\{0,2};\=\%(1;\)\=44\%(1;\)\=m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiMagentaBg	start="\e\[;\=0\{0,2};\=\%(1;\)\=45\%(1;\)\=m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiCyanBg		start="\e\[;\=0\{0,2};\=\%(1;\)\=46\%(1;\)\=m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiWhiteBg	start="\e\[;\=0\{0,2};\=\%(1;\)\=47\%(1;\)\=m" end="\e\["me=e-2 contains=ansiConceal

  syn region ansiBoldBlack	 start="\e\[;\=0\{0,2};\=\%(1;30\|30;1\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBoldRed	 start="\e\[;\=0\{0,2};\=\%(1;31\|31;1\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBoldGreen	 start="\e\[;\=0\{0,2};\=\%(1;32\|32;1\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBoldYellow	 start="\e\[;\=0\{0,2};\=\%(1;33\|33;1\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBoldBlue	 start="\e\[;\=0\{0,2};\=\%(1;34\|34;1\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBoldMagenta	 start="\e\[;\=0\{0,2};\=\%(1;35\|35;1\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBoldCyan	 start="\e\[;\=0\{0,2};\=\%(1;36\|36;1\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBoldWhite	 start="\e\[;\=0\{0,2};\=\%(1;37\|37;1\)m" end="\e\["me=e-2 contains=ansiConceal

  syn region ansiStandoutBlack	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(3;30\|30;3\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiStandoutRed	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(3;31\|31;3\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiStandoutGreen	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(3;32\|32;3\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiStandoutYellow	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(3;33\|33;3\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiStandoutBlue	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(3;34\|34;3\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiStandoutMagenta	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(3;35\|35;3\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiStandoutCyan	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(3;36\|36;3\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiStandoutWhite	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(3;37\|37;3\)m" end="\e\["me=e-2 contains=ansiConceal

  syn region ansiItalicBlack	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(2;30\|30;2\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiItalicRed	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(2;31\|31;2\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiItalicGreen	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(2;32\|32;2\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiItalicYellow	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(2;33\|33;2\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiItalicBlue	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(2;34\|34;2\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiItalicMagenta	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(2;35\|35;2\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiItalicCyan	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(2;36\|36;2\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiItalicWhite	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(2;37\|37;2\)m" end="\e\["me=e-2 contains=ansiConceal

  syn region ansiUnderlineBlack	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(4;30\|30;4\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiUnderlineRed	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(4;31\|31;4\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiUnderlineGreen	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(4;32\|32;4\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiUnderlineYellow	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(4;33\|33;4\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiUnderlineBlue	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(4;34\|34;4\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiUnderlineMagenta	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(4;35\|35;4\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiUnderlineCyan	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(4;36\|36;4\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiUnderlineWhite	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(4;37\|37;4\)m" end="\e\["me=e-2 contains=ansiConceal

  syn region ansiBlinkBlack	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(5;30\|30;5\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlinkRed	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(5;31\|31;5\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlinkGreen	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(5;32\|32;5\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlinkYellow	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(5;33\|33;5\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlinkBlue	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(5;34\|34;5\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlinkMagenta	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(5;35\|35;5\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlinkCyan	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(5;36\|36;5\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlinkWhite	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(5;37\|37;5\)m" end="\e\["me=e-2 contains=ansiConceal

  syn region ansiRapidBlinkBlack	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(6;30\|30;6\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRapidBlinkRed	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(6;31\|31;6\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRapidBlinkGreen	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(6;32\|32;6\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRapidBlinkYellow	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(6;33\|33;6\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRapidBlinkBlue	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(6;34\|34;6\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRapidBlinkMagenta	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(6;35\|35;6\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRapidBlinkCyan	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(6;36\|36;6\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRapidBlinkWhite	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(6;37\|37;6\)m" end="\e\["me=e-2 contains=ansiConceal

  syn region ansiRVBlack	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(7;30\|30;7\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRVRed		 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(7;31\|31;7\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRVGreen	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(7;32\|32;7\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRVYellow	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(7;33\|33;7\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRVBlue		 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(7;34\|34;7\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRVMagenta	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(7;35\|35;7\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRVCyan		 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(7;36\|36;7\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRVWhite	 start="\e\[;\=0\{0,2};\=\%(1;\)\=\%(7;37\|37;7\)m" end="\e\["me=e-2 contains=ansiConceal

  if has("conceal")
   syn match ansiStop		conceal "\e\[;\=0\{1,2}m"
   syn match ansiStop		conceal "\e\[K"
   syn match ansiStop		conceal "\e\[H"
   syn match ansiStop		conceal "\e\[2J"
  else
   syn match ansiStop		"\e\[;\=0\{0,2}m"
   syn match ansiStop		"\e\[K"
   syn match ansiStop		"\e\[H"
   syn match ansiStop		"\e\[2J"
  endif

  "syn match ansiIgnore		conceal "\e\[\([56];3[0-9]\|3[0-9];[56]\)m"
  "syn match ansiIgnore		conceal "\e\[\([0-9]\+;\)\{2,}[0-9]\+m"

  " ---------------------------------------------------------------------
  " Some Color Combinations: - can't do 'em all, the qty of highlighting groups is limited! {{{2
  " ---------------------------------------------------------------------
  syn region ansiBlackBlack	 start="\e\[0\{0,2};\=\(30;40\|40;30\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRedBlack	 start="\e\[0\{0,2};\=\(31;40\|40;31\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiGreenBlack	 start="\e\[0\{0,2};\=\(32;40\|40;32\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiYellowBlack	 start="\e\[0\{0,2};\=\(33;40\|40;33\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlueBlack	 start="\e\[0\{0,2};\=\(34;40\|40;34\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiMagentaBlack	 start="\e\[0\{0,2};\=\(35;40\|40;35\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiCyanBlack	 start="\e\[0\{0,2};\=\(36;40\|40;36\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiWhiteBlack	 start="\e\[0\{0,2};\=\(37;40\|40;37\)m" end="\e\["me=e-2 contains=ansiConceal

  syn region ansiBlackRed	 start="\e\[0\{0,2};\=\(30;41\|41;30\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRedRed		 start="\e\[0\{0,2};\=\(31;41\|41;31\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiGreenRed	 start="\e\[0\{0,2};\=\(32;41\|41;32\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiYellowRed	 start="\e\[0\{0,2};\=\(33;41\|41;33\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlueRed	 start="\e\[0\{0,2};\=\(34;41\|41;34\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiMagentaRed	 start="\e\[0\{0,2};\=\(35;41\|41;35\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiCyanRed	 start="\e\[0\{0,2};\=\(36;41\|41;36\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiWhiteRed	 start="\e\[0\{0,2};\=\(37;41\|41;37\)m" end="\e\["me=e-2 contains=ansiConceal

  syn region ansiBlackGreen	 start="\e\[0\{0,2};\=\(30;42\|42;30\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRedGreen	 start="\e\[0\{0,2};\=\(31;42\|42;31\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiGreenGreen	 start="\e\[0\{0,2};\=\(32;42\|42;32\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiYellowGreen	 start="\e\[0\{0,2};\=\(33;42\|42;33\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlueGreen	 start="\e\[0\{0,2};\=\(34;42\|42;34\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiMagentaGreen	 start="\e\[0\{0,2};\=\(35;42\|42;35\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiCyanGreen	 start="\e\[0\{0,2};\=\(36;42\|42;36\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiWhiteGreen	 start="\e\[0\{0,2};\=\(37;42\|42;37\)m" end="\e\["me=e-2 contains=ansiConceal

  syn region ansiBlackYellow	 start="\e\[0\{0,2};\=\(30;43\|43;30\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRedYellow	 start="\e\[0\{0,2};\=\(31;43\|43;31\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiGreenYellow	 start="\e\[0\{0,2};\=\(32;43\|43;32\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiYellowYellow	 start="\e\[0\{0,2};\=\(33;43\|43;33\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlueYellow	 start="\e\[0\{0,2};\=\(34;43\|43;34\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiMagentaYellow	 start="\e\[0\{0,2};\=\(35;43\|43;35\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiCyanYellow	 start="\e\[0\{0,2};\=\(36;43\|43;36\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiWhiteYellow	 start="\e\[0\{0,2};\=\(37;43\|43;37\)m" end="\e\["me=e-2 contains=ansiConceal

  syn region ansiBlackBlue	 start="\e\[0\{0,2};\=\(30;44\|44;30\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRedBlue	 start="\e\[0\{0,2};\=\(31;44\|44;31\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiGreenBlue	 start="\e\[0\{0,2};\=\(32;44\|44;32\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiYellowBlue	 start="\e\[0\{0,2};\=\(33;44\|44;33\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlueBlue	 start="\e\[0\{0,2};\=\(34;44\|44;34\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiMagentaBlue	 start="\e\[0\{0,2};\=\(35;44\|44;35\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiCyanBlue	 start="\e\[0\{0,2};\=\(36;44\|44;36\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiWhiteBlue	 start="\e\[0\{0,2};\=\(37;44\|44;37\)m" end="\e\["me=e-2 contains=ansiConceal

  syn region ansiBlackMagenta	 start="\e\[0\{0,2};\=\(30;45\|45;30\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRedMagenta	 start="\e\[0\{0,2};\=\(31;45\|45;31\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiGreenMagenta	 start="\e\[0\{0,2};\=\(32;45\|45;32\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiYellowMagenta	 start="\e\[0\{0,2};\=\(33;45\|45;33\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlueMagenta	 start="\e\[0\{0,2};\=\(34;45\|45;34\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiMagentaMagenta	 start="\e\[0\{0,2};\=\(35;45\|45;35\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiCyanMagenta	 start="\e\[0\{0,2};\=\(36;45\|45;36\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiWhiteMagenta	 start="\e\[0\{0,2};\=\(37;45\|45;37\)m" end="\e\["me=e-2 contains=ansiConceal

  syn region ansiBlackCyan	 start="\e\[0\{0,2};\=\(30;46\|46;30\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRedCyan	 start="\e\[0\{0,2};\=\(31;46\|46;31\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiGreenCyan	 start="\e\[0\{0,2};\=\(32;46\|46;32\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiYellowCyan	 start="\e\[0\{0,2};\=\(33;46\|46;33\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlueCyan	 start="\e\[0\{0,2};\=\(34;46\|46;34\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiMagentaCyan	 start="\e\[0\{0,2};\=\(35;46\|46;35\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiCyanCyan	 start="\e\[0\{0,2};\=\(36;46\|46;36\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiWhiteCyan	 start="\e\[0\{0,2};\=\(37;46\|46;37\)m" end="\e\["me=e-2 contains=ansiConceal

  syn region ansiBlackWhite	 start="\e\[0\{0,2};\=\(30;47\|47;30\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiRedWhite	 start="\e\[0\{0,2};\=\(31;47\|47;31\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiGreenWhite	 start="\e\[0\{0,2};\=\(32;47\|47;32\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiYellowWhite	 start="\e\[0\{0,2};\=\(33;47\|47;33\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiBlueWhite	 start="\e\[0\{0,2};\=\(34;47\|47;34\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiMagentaWhite	 start="\e\[0\{0,2};\=\(35;47\|47;35\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiCyanWhite	 start="\e\[0\{0,2};\=\(36;47\|47;36\)m" end="\e\["me=e-2 contains=ansiConceal
  syn region ansiWhiteWhite	 start="\e\[0\{0,2};\=\(37;47\|47;37\)m" end="\e\["me=e-2 contains=ansiConceal

  syn match ansiExtended	 "\e\[;\=\(0;\)\=[34]8;\(\d*;\)*\d*m"   contains=ansiConceal

  if has("conceal")
   syn match ansiConceal		contained conceal	"\e\[\(\d*;\)*\d*m"
  else
   syn match ansiConceal		contained		"\e\[\(\d*;\)*\d*m"
  endif

  " -------------
  " Highlighting: {{{2
  " -------------
  if !has("conceal")
   " --------------
   " ansiesc_ignore: {{{3
   " --------------
   hi def link ansiConceal	Ignore
   hi def link ansiSuppress	Ignore
   hi def link ansiIgnore	ansiStop
   hi def link ansiStop		Ignore
   hi def link ansiExtended	Ignore
  endif
  let s:hlkeep_{bufnr("%")}= &l:hl
  exe "setlocal hl=".substitute(&hl,'8:[^,]\{-},','8:Ignore,',"")

  " handle 3 or more element ansi escape sequences by building syntax and highlighting rules
  " specific to the current file
  call s:MultiElementHandler()

  hi ansiNone	cterm=NONE gui=NONE

  if &t_Co == 8 || &t_Co == 256
   " ---------------------
   " eight-color handling: {{{3
   " ---------------------
"   call Decho("set up 8-color highlighting groups")
   hi ansiBlack             ctermfg=black      guifg=black                                        cterm=none         gui=none
   hi ansiRed               ctermfg=red        guifg=red                                          cterm=none         gui=none
   hi ansiGreen             ctermfg=green      guifg=green                                        cterm=none         gui=none
   hi ansiYellow            ctermfg=yellow     guifg=yellow                                       cterm=none         gui=none
   hi ansiBlue              ctermfg=blue       guifg=blue                                         cterm=none         gui=none
   hi ansiMagenta           ctermfg=magenta    guifg=magenta                                      cterm=none         gui=none
   hi ansiCyan              ctermfg=cyan       guifg=cyan                                         cterm=none         gui=none
   hi ansiWhite             ctermfg=white      guifg=white                                        cterm=none         gui=none

   hi ansiBlackBg           ctermbg=black      guibg=black                                        cterm=none         gui=none
   hi ansiRedBg             ctermbg=red        guibg=red                                          cterm=none         gui=none
   hi ansiGreenBg           ctermbg=green      guibg=green                                        cterm=none         gui=none
   hi ansiYellowBg          ctermbg=yellow     guibg=yellow                                       cterm=none         gui=none
   hi ansiBlueBg            ctermbg=blue       guibg=blue                                         cterm=none         gui=none
   hi ansiMagentaBg         ctermbg=magenta    guibg=magenta                                      cterm=none         gui=none
   hi ansiCyanBg            ctermbg=cyan       guibg=cyan                                         cterm=none         gui=none
   hi ansiWhiteBg           ctermbg=white      guibg=white                                        cterm=none         gui=none

   hi ansiBoldBlack         ctermfg=black      guifg=black                                        cterm=bold         gui=bold
   hi ansiBoldRed           ctermfg=red        guifg=red                                          cterm=bold         gui=bold
   hi ansiBoldGreen         ctermfg=green      guifg=green                                        cterm=bold         gui=bold
   hi ansiBoldYellow        ctermfg=yellow     guifg=yellow                                       cterm=bold         gui=bold
   hi ansiBoldBlue          ctermfg=blue       guifg=blue                                         cterm=bold         gui=bold
   hi ansiBoldMagenta       ctermfg=magenta    guifg=magenta                                      cterm=bold         gui=bold
   hi ansiBoldCyan          ctermfg=cyan       guifg=cyan                                         cterm=bold         gui=bold
   hi ansiBoldWhite         ctermfg=white      guifg=white                                        cterm=bold         gui=bold

   hi ansiStandoutBlack     ctermfg=black      guifg=black                                        cterm=standout     gui=standout
   hi ansiStandoutRed       ctermfg=red        guifg=red                                          cterm=standout     gui=standout
   hi ansiStandoutGreen     ctermfg=green      guifg=green                                        cterm=standout     gui=standout
   hi ansiStandoutYellow    ctermfg=yellow     guifg=yellow                                       cterm=standout     gui=standout
   hi ansiStandoutBlue      ctermfg=blue       guifg=blue                                         cterm=standout     gui=standout
   hi ansiStandoutMagenta   ctermfg=magenta    guifg=magenta                                      cterm=standout     gui=standout
   hi ansiStandoutCyan      ctermfg=cyan       guifg=cyan                                         cterm=standout     gui=standout
   hi ansiStandoutWhite     ctermfg=white      guifg=white                                        cterm=standout     gui=standout

   hi ansiItalicBlack       ctermfg=black      guifg=black                                        cterm=italic       gui=italic
   hi ansiItalicRed         ctermfg=red        guifg=red                                          cterm=italic       gui=italic
   hi ansiItalicGreen       ctermfg=green      guifg=green                                        cterm=italic       gui=italic
   hi ansiItalicYellow      ctermfg=yellow     guifg=yellow                                       cterm=italic       gui=italic
   hi ansiItalicBlue        ctermfg=blue       guifg=blue                                         cterm=italic       gui=italic
   hi ansiItalicMagenta     ctermfg=magenta    guifg=magenta                                      cterm=italic       gui=italic
   hi ansiItalicCyan        ctermfg=cyan       guifg=cyan                                         cterm=italic       gui=italic
   hi ansiItalicWhite       ctermfg=white      guifg=white                                        cterm=italic       gui=italic

   hi ansiUnderlineBlack    ctermfg=black      guifg=black                                        cterm=underline    gui=underline
   hi ansiUnderlineRed      ctermfg=red        guifg=red                                          cterm=underline    gui=underline
   hi ansiUnderlineGreen    ctermfg=green      guifg=green                                        cterm=underline    gui=underline
   hi ansiUnderlineYellow   ctermfg=yellow     guifg=yellow                                       cterm=underline    gui=underline
   hi ansiUnderlineBlue     ctermfg=blue       guifg=blue                                         cterm=underline    gui=underline
   hi ansiUnderlineMagenta  ctermfg=magenta    guifg=magenta                                      cterm=underline    gui=underline
   hi ansiUnderlineCyan     ctermfg=cyan       guifg=cyan                                         cterm=underline    gui=underline
   hi ansiUnderlineWhite    ctermfg=white      guifg=white                                        cterm=underline    gui=underline

   hi ansiBlinkBlack        ctermfg=black      guifg=black                                        cterm=standout     gui=undercurl
   hi ansiBlinkRed          ctermfg=red        guifg=red                                          cterm=standout     gui=undercurl
   hi ansiBlinkGreen        ctermfg=green      guifg=green                                        cterm=standout     gui=undercurl
   hi ansiBlinkYellow       ctermfg=yellow     guifg=yellow                                       cterm=standout     gui=undercurl
   hi ansiBlinkBlue         ctermfg=blue       guifg=blue                                         cterm=standout     gui=undercurl
   hi ansiBlinkMagenta      ctermfg=magenta    guifg=magenta                                      cterm=standout     gui=undercurl
   hi ansiBlinkCyan         ctermfg=cyan       guifg=cyan                                         cterm=standout     gui=undercurl
   hi ansiBlinkWhite        ctermfg=white      guifg=white                                        cterm=standout     gui=undercurl

   hi ansiRapidBlinkBlack   ctermfg=black      guifg=black                                        cterm=standout     gui=undercurl
   hi ansiRapidBlinkRed     ctermfg=red        guifg=red                                          cterm=standout     gui=undercurl
   hi ansiRapidBlinkGreen   ctermfg=green      guifg=green                                        cterm=standout     gui=undercurl
   hi ansiRapidBlinkYellow  ctermfg=yellow     guifg=yellow                                       cterm=standout     gui=undercurl
   hi ansiRapidBlinkBlue    ctermfg=blue       guifg=blue                                         cterm=standout     gui=undercurl
   hi ansiRapidBlinkMagenta ctermfg=magenta    guifg=magenta                                      cterm=standout     gui=undercurl
   hi ansiRapidBlinkCyan    ctermfg=cyan       guifg=cyan                                         cterm=standout     gui=undercurl
   hi ansiRapidBlinkWhite   ctermfg=white      guifg=white                                        cterm=standout     gui=undercurl

   hi ansiRVBlack           ctermfg=black      guifg=black                                        cterm=reverse      gui=reverse
   hi ansiRVRed             ctermfg=red        guifg=red                                          cterm=reverse      gui=reverse
   hi ansiRVGreen           ctermfg=green      guifg=green                                        cterm=reverse      gui=reverse
   hi ansiRVYellow          ctermfg=yellow     guifg=yellow                                       cterm=reverse      gui=reverse
   hi ansiRVBlue            ctermfg=blue       guifg=blue                                         cterm=reverse      gui=reverse
   hi ansiRVMagenta         ctermfg=magenta    guifg=magenta                                      cterm=reverse      gui=reverse
   hi ansiRVCyan            ctermfg=cyan       guifg=cyan                                         cterm=reverse      gui=reverse
   hi ansiRVWhite           ctermfg=white      guifg=white                                        cterm=reverse      gui=reverse

   hi ansiBlackBlack        ctermfg=black      ctermbg=black      guifg=Black      guibg=Black    cterm=none         gui=none
   hi ansiRedBlack          ctermfg=red        ctermbg=black      guifg=Red        guibg=Black    cterm=none         gui=none
   hi ansiGreenBlack        ctermfg=green      ctermbg=black      guifg=Green      guibg=Black    cterm=none         gui=none
   hi ansiYellowBlack       ctermfg=yellow     ctermbg=black      guifg=Yellow     guibg=Black    cterm=none         gui=none
   hi ansiBlueBlack         ctermfg=blue       ctermbg=black      guifg=Blue       guibg=Black    cterm=none         gui=none
   hi ansiMagentaBlack      ctermfg=magenta    ctermbg=black      guifg=Magenta    guibg=Black    cterm=none         gui=none
   hi ansiCyanBlack         ctermfg=cyan       ctermbg=black      guifg=Cyan       guibg=Black    cterm=none         gui=none
   hi ansiWhiteBlack        ctermfg=white      ctermbg=black      guifg=White      guibg=Black    cterm=none         gui=none

   hi ansiBlackRed          ctermfg=black      ctermbg=red        guifg=Black      guibg=Red      cterm=none         gui=none
   hi ansiRedRed            ctermfg=red        ctermbg=red        guifg=Red        guibg=Red      cterm=none         gui=none
   hi ansiGreenRed          ctermfg=green      ctermbg=red        guifg=Green      guibg=Red      cterm=none         gui=none
   hi ansiYellowRed         ctermfg=yellow     ctermbg=red        guifg=Yellow     guibg=Red      cterm=none         gui=none
   hi ansiBlueRed           ctermfg=blue       ctermbg=red        guifg=Blue       guibg=Red      cterm=none         gui=none
   hi ansiMagentaRed        ctermfg=magenta    ctermbg=red        guifg=Magenta    guibg=Red      cterm=none         gui=none
   hi ansiCyanRed           ctermfg=cyan       ctermbg=red        guifg=Cyan       guibg=Red      cterm=none         gui=none
   hi ansiWhiteRed          ctermfg=white      ctermbg=red        guifg=White      guibg=Red      cterm=none         gui=none

   hi ansiBlackGreen        ctermfg=black      ctermbg=green      guifg=Black      guibg=Green    cterm=none         gui=none
   hi ansiRedGreen          ctermfg=red        ctermbg=green      guifg=Red        guibg=Green    cterm=none         gui=none
   hi ansiGreenGreen        ctermfg=green      ctermbg=green      guifg=Green      guibg=Green    cterm=none         gui=none
   hi ansiYellowGreen       ctermfg=yellow     ctermbg=green      guifg=Yellow     guibg=Green    cterm=none         gui=none
   hi ansiBlueGreen         ctermfg=blue       ctermbg=green      guifg=Blue       guibg=Green    cterm=none         gui=none
   hi ansiMagentaGreen      ctermfg=magenta    ctermbg=green      guifg=Magenta    guibg=Green    cterm=none         gui=none
   hi ansiCyanGreen         ctermfg=cyan       ctermbg=green      guifg=Cyan       guibg=Green    cterm=none         gui=none
   hi ansiWhiteGreen        ctermfg=white      ctermbg=green      guifg=White      guibg=Green    cterm=none         gui=none

   hi ansiBlackYellow       ctermfg=black      ctermbg=yellow     guifg=Black      guibg=Yellow   cterm=none         gui=none
   hi ansiRedYellow         ctermfg=red        ctermbg=yellow     guifg=Red        guibg=Yellow   cterm=none         gui=none
   hi ansiGreenYellow       ctermfg=green      ctermbg=yellow     guifg=Green      guibg=Yellow   cterm=none         gui=none
   hi ansiYellowYellow      ctermfg=yellow     ctermbg=yellow     guifg=Yellow     guibg=Yellow   cterm=none         gui=none
   hi ansiBlueYellow        ctermfg=blue       ctermbg=yellow     guifg=Blue       guibg=Yellow   cterm=none         gui=none
   hi ansiMagentaYellow     ctermfg=magenta    ctermbg=yellow     guifg=Magenta    guibg=Yellow   cterm=none         gui=none
   hi ansiCyanYellow        ctermfg=cyan       ctermbg=yellow     guifg=Cyan       guibg=Yellow   cterm=none         gui=none
   hi ansiWhiteYellow       ctermfg=white      ctermbg=yellow     guifg=White      guibg=Yellow   cterm=none         gui=none

   hi ansiBlackBlue         ctermfg=black      ctermbg=blue       guifg=Black      guibg=Blue     cterm=none         gui=none
   hi ansiRedBlue           ctermfg=red        ctermbg=blue       guifg=Red        guibg=Blue     cterm=none         gui=none
   hi ansiGreenBlue         ctermfg=green      ctermbg=blue       guifg=Green      guibg=Blue     cterm=none         gui=none
   hi ansiYellowBlue        ctermfg=yellow     ctermbg=blue       guifg=Yellow     guibg=Blue     cterm=none         gui=none
   hi ansiBlueBlue          ctermfg=blue       ctermbg=blue       guifg=Blue       guibg=Blue     cterm=none         gui=none
   hi ansiMagentaBlue       ctermfg=magenta    ctermbg=blue       guifg=Magenta    guibg=Blue     cterm=none         gui=none
   hi ansiCyanBlue          ctermfg=cyan       ctermbg=blue       guifg=Cyan       guibg=Blue     cterm=none         gui=none
   hi ansiWhiteBlue         ctermfg=white      ctermbg=blue       guifg=White      guibg=Blue     cterm=none         gui=none

   hi ansiBlackMagenta      ctermfg=black      ctermbg=magenta    guifg=Black      guibg=Magenta  cterm=none         gui=none
   hi ansiRedMagenta        ctermfg=red        ctermbg=magenta    guifg=Red        guibg=Magenta  cterm=none         gui=none
   hi ansiGreenMagenta      ctermfg=green      ctermbg=magenta    guifg=Green      guibg=Magenta  cterm=none         gui=none
   hi ansiYellowMagenta     ctermfg=yellow     ctermbg=magenta    guifg=Yellow     guibg=Magenta  cterm=none         gui=none
   hi ansiBlueMagenta       ctermfg=blue       ctermbg=magenta    guifg=Blue       guibg=Magenta  cterm=none         gui=none
   hi ansiMagentaMagenta    ctermfg=magenta    ctermbg=magenta    guifg=Magenta    guibg=Magenta  cterm=none         gui=none
   hi ansiCyanMagenta       ctermfg=cyan       ctermbg=magenta    guifg=Cyan       guibg=Magenta  cterm=none         gui=none
   hi ansiWhiteMagenta      ctermfg=white      ctermbg=magenta    guifg=White      guibg=Magenta  cterm=none         gui=none

   hi ansiBlackCyan         ctermfg=black      ctermbg=cyan       guifg=Black      guibg=Cyan     cterm=none         gui=none
   hi ansiRedCyan           ctermfg=red        ctermbg=cyan       guifg=Red        guibg=Cyan     cterm=none         gui=none
   hi ansiGreenCyan         ctermfg=green      ctermbg=cyan       guifg=Green      guibg=Cyan     cterm=none         gui=none
   hi ansiYellowCyan        ctermfg=yellow     ctermbg=cyan       guifg=Yellow     guibg=Cyan     cterm=none         gui=none
   hi ansiBlueCyan          ctermfg=blue       ctermbg=cyan       guifg=Blue       guibg=Cyan     cterm=none         gui=none
   hi ansiMagentaCyan       ctermfg=magenta    ctermbg=cyan       guifg=Magenta    guibg=Cyan     cterm=none         gui=none
   hi ansiCyanCyan          ctermfg=cyan       ctermbg=cyan       guifg=Cyan       guibg=Cyan     cterm=none         gui=none
   hi ansiWhiteCyan         ctermfg=white      ctermbg=cyan       guifg=White      guibg=Cyan     cterm=none         gui=none

   hi ansiBlackWhite        ctermfg=black      ctermbg=white      guifg=Black      guibg=White    cterm=none         gui=none
   hi ansiRedWhite          ctermfg=red        ctermbg=white      guifg=Red        guibg=White    cterm=none         gui=none
   hi ansiGreenWhite        ctermfg=green      ctermbg=white      guifg=Green      guibg=White    cterm=none         gui=none
   hi ansiYellowWhite       ctermfg=yellow     ctermbg=white      guifg=Yellow     guibg=White    cterm=none         gui=none
   hi ansiBlueWhite         ctermfg=blue       ctermbg=white      guifg=Blue       guibg=White    cterm=none         gui=none
   hi ansiMagentaWhite      ctermfg=magenta    ctermbg=white      guifg=Magenta    guibg=White    cterm=none         gui=none
   hi ansiCyanWhite         ctermfg=cyan       ctermbg=white      guifg=Cyan       guibg=White    cterm=none         gui=none
   hi ansiWhiteWhite        ctermfg=white      ctermbg=white      guifg=White      guibg=White    cterm=none         gui=none

   if v:version >= 700 && exists("&t_Co") && &t_Co == 256 && exists("g:ansiesc_256color")
    " ---------------------------
    " handle 256-color terminals: {{{3
    " ---------------------------
"    call Decho("set up 256-color highlighting groups")
    let icolor= 1
    while icolor < 256
     let jcolor= 1
     exe "hi ansiHL_".icolor."_0 ctermfg=".icolor
     exe "hi ansiHL_0_".icolor." ctermbg=".icolor
"     call Decho("exe hi ansiHL_".icolor." ctermfg=".icolor)
     while jcolor < 256
      exe "hi ansiHL_".icolor."_".jcolor." ctermfg=".icolor." ctermbg=".jcolor
"      call Decho("exe hi ansiHL_".icolor."_".jcolor." ctermfg=".icolor." ctermbg=".jcolor)
      let jcolor= jcolor + 1
     endwhile
     let icolor= icolor + 1
    endwhile
   endif

  else
   " ----------------------------------
   " not 8 or 256 color terminals (gui): {{{3
   " ----------------------------------
"   call Decho("set up gui highlighting groups")
   hi ansiBlack             ctermfg=black      guifg=black                                        cterm=none         gui=none
   hi ansiRed               ctermfg=red        guifg=red                                          cterm=none         gui=none
   hi ansiGreen             ctermfg=green      guifg=green                                        cterm=none         gui=none
   hi ansiYellow            ctermfg=yellow     guifg=yellow                                       cterm=none         gui=none
   hi ansiBlue              ctermfg=blue       guifg=blue                                         cterm=none         gui=none
   hi ansiMagenta           ctermfg=magenta    guifg=magenta                                      cterm=none         gui=none
   hi ansiCyan              ctermfg=cyan       guifg=cyan                                         cterm=none         gui=none
   hi ansiWhite             ctermfg=white      guifg=white                                        cterm=none         gui=none

   hi ansiBlackBg           ctermbg=black      guibg=black                                        cterm=none         gui=none
   hi ansiRedBg             ctermbg=red        guibg=red                                          cterm=none         gui=none
   hi ansiGreenBg           ctermbg=green      guibg=green                                        cterm=none         gui=none
   hi ansiYellowBg          ctermbg=yellow     guibg=yellow                                       cterm=none         gui=none
   hi ansiBlueBg            ctermbg=blue       guibg=blue                                         cterm=none         gui=none
   hi ansiMagentaBg         ctermbg=magenta    guibg=magenta                                      cterm=none         gui=none
   hi ansiCyanBg            ctermbg=cyan       guibg=cyan                                         cterm=none         gui=none
   hi ansiWhiteBg           ctermbg=white      guibg=white                                        cterm=none         gui=none

   hi ansiBoldBlack         ctermfg=black      guifg=black                                        cterm=bold         gui=bold
   hi ansiBoldRed           ctermfg=red        guifg=red                                          cterm=bold         gui=bold
   hi ansiBoldGreen         ctermfg=green      guifg=green                                        cterm=bold         gui=bold
   hi ansiBoldYellow        ctermfg=yellow     guifg=yellow                                       cterm=bold         gui=bold
   hi ansiBoldBlue          ctermfg=blue       guifg=blue                                         cterm=bold         gui=bold
   hi ansiBoldMagenta       ctermfg=magenta    guifg=magenta                                      cterm=bold         gui=bold
   hi ansiBoldCyan          ctermfg=cyan       guifg=cyan                                         cterm=bold         gui=bold
   hi ansiBoldWhite         ctermfg=white      guifg=white                                        cterm=bold         gui=bold

   hi ansiStandoutBlack     ctermfg=black      guifg=black                                        cterm=standout     gui=standout
   hi ansiStandoutRed       ctermfg=red        guifg=red                                          cterm=standout     gui=standout
   hi ansiStandoutGreen     ctermfg=green      guifg=green                                        cterm=standout     gui=standout
   hi ansiStandoutYellow    ctermfg=yellow     guifg=yellow                                       cterm=standout     gui=standout
   hi ansiStandoutBlue      ctermfg=blue       guifg=blue                                         cterm=standout     gui=standout
   hi ansiStandoutMagenta   ctermfg=magenta    guifg=magenta                                      cterm=standout     gui=standout
   hi ansiStandoutCyan      ctermfg=cyan       guifg=cyan                                         cterm=standout     gui=standout
   hi ansiStandoutWhite     ctermfg=white      guifg=white                                        cterm=standout     gui=standout

   hi ansiItalicBlack       ctermfg=black      guifg=black                                        cterm=italic       gui=italic
   hi ansiItalicRed         ctermfg=red        guifg=red                                          cterm=italic       gui=italic
   hi ansiItalicGreen       ctermfg=green      guifg=green                                        cterm=italic       gui=italic
   hi ansiItalicYellow      ctermfg=yellow     guifg=yellow                                       cterm=italic       gui=italic
   hi ansiItalicBlue        ctermfg=blue       guifg=blue                                         cterm=italic       gui=italic
   hi ansiItalicMagenta     ctermfg=magenta    guifg=magenta                                      cterm=italic       gui=italic
   hi ansiItalicCyan        ctermfg=cyan       guifg=cyan                                         cterm=italic       gui=italic
   hi ansiItalicWhite       ctermfg=white      guifg=white                                        cterm=italic       gui=italic

   hi ansiUnderlineBlack    ctermfg=black      guifg=black                                        cterm=underline    gui=underline
   hi ansiUnderlineRed      ctermfg=red        guifg=red                                          cterm=underline    gui=underline
   hi ansiUnderlineGreen    ctermfg=green      guifg=green                                        cterm=underline    gui=underline
   hi ansiUnderlineYellow   ctermfg=yellow     guifg=yellow                                       cterm=underline    gui=underline
   hi ansiUnderlineBlue     ctermfg=blue       guifg=blue                                         cterm=underline    gui=underline
   hi ansiUnderlineMagenta  ctermfg=magenta    guifg=magenta                                      cterm=underline    gui=underline
   hi ansiUnderlineCyan     ctermfg=cyan       guifg=cyan                                         cterm=underline    gui=underline
   hi ansiUnderlineWhite    ctermfg=white      guifg=white                                        cterm=underline    gui=underline

   hi ansiBlinkBlack        ctermfg=black      guifg=black                                        cterm=standout     gui=undercurl
   hi ansiBlinkRed          ctermfg=red        guifg=red                                          cterm=standout     gui=undercurl
   hi ansiBlinkGreen        ctermfg=green      guifg=green                                        cterm=standout     gui=undercurl
   hi ansiBlinkYellow       ctermfg=yellow     guifg=yellow                                       cterm=standout     gui=undercurl
   hi ansiBlinkBlue         ctermfg=blue       guifg=blue                                         cterm=standout     gui=undercurl
   hi ansiBlinkMagenta      ctermfg=magenta    guifg=magenta                                      cterm=standout     gui=undercurl
   hi ansiBlinkCyan         ctermfg=cyan       guifg=cyan                                         cterm=standout     gui=undercurl
   hi ansiBlinkWhite        ctermfg=white      guifg=white                                        cterm=standout     gui=undercurl

   hi ansiRapidBlinkBlack   ctermfg=black      guifg=black                                        cterm=standout     gui=undercurl
   hi ansiRapidBlinkRed     ctermfg=red        guifg=red                                          cterm=standout     gui=undercurl
   hi ansiRapidBlinkGreen   ctermfg=green      guifg=green                                        cterm=standout     gui=undercurl
   hi ansiRapidBlinkYellow  ctermfg=yellow     guifg=yellow                                       cterm=standout     gui=undercurl
   hi ansiRapidBlinkBlue    ctermfg=blue       guifg=blue                                         cterm=standout     gui=undercurl
   hi ansiRapidBlinkMagenta ctermfg=magenta    guifg=magenta                                      cterm=standout     gui=undercurl
   hi ansiRapidBlinkCyan    ctermfg=cyan       guifg=cyan                                         cterm=standout     gui=undercurl
   hi ansiRapidBlinkWhite   ctermfg=white      guifg=white                                        cterm=standout     gui=undercurl

   hi ansiRVBlack           ctermfg=black      guifg=black                                        cterm=reverse      gui=reverse
   hi ansiRVRed             ctermfg=red        guifg=red                                          cterm=reverse      gui=reverse
   hi ansiRVGreen           ctermfg=green      guifg=green                                        cterm=reverse      gui=reverse
   hi ansiRVYellow          ctermfg=yellow     guifg=yellow                                       cterm=reverse      gui=reverse
   hi ansiRVBlue            ctermfg=blue       guifg=blue                                         cterm=reverse      gui=reverse
   hi ansiRVMagenta         ctermfg=magenta    guifg=magenta                                      cterm=reverse      gui=reverse
   hi ansiRVCyan            ctermfg=cyan       guifg=cyan                                         cterm=reverse      gui=reverse
   hi ansiRVWhite           ctermfg=white      guifg=white                                        cterm=reverse      gui=reverse

   hi ansiBlackBlack        ctermfg=black      ctermbg=black      guifg=Black      guibg=Black    cterm=none         gui=none
   hi ansiRedBlack          ctermfg=black      ctermbg=black      guifg=Black      guibg=Black    cterm=none         gui=none
   hi ansiRedBlack          ctermfg=red        ctermbg=black      guifg=Red        guibg=Black    cterm=none         gui=none
   hi ansiGreenBlack        ctermfg=green      ctermbg=black      guifg=Green      guibg=Black    cterm=none         gui=none
   hi ansiYellowBlack       ctermfg=yellow     ctermbg=black      guifg=Yellow     guibg=Black    cterm=none         gui=none
   hi ansiBlueBlack         ctermfg=blue       ctermbg=black      guifg=Blue       guibg=Black    cterm=none         gui=none
   hi ansiMagentaBlack      ctermfg=magenta    ctermbg=black      guifg=Magenta    guibg=Black    cterm=none         gui=none
   hi ansiCyanBlack         ctermfg=cyan       ctermbg=black      guifg=Cyan       guibg=Black    cterm=none         gui=none
   hi ansiWhiteBlack        ctermfg=white      ctermbg=black      guifg=White      guibg=Black    cterm=none         gui=none

   hi ansiBlackRed          ctermfg=black      ctermbg=red        guifg=Black      guibg=Red      cterm=none         gui=none
   hi ansiRedRed            ctermfg=red        ctermbg=red        guifg=Red        guibg=Red      cterm=none         gui=none
   hi ansiGreenRed          ctermfg=green      ctermbg=red        guifg=Green      guibg=Red      cterm=none         gui=none
   hi ansiYellowRed         ctermfg=yellow     ctermbg=red        guifg=Yellow     guibg=Red      cterm=none         gui=none
   hi ansiBlueRed           ctermfg=blue       ctermbg=red        guifg=Blue       guibg=Red      cterm=none         gui=none
   hi ansiMagentaRed        ctermfg=magenta    ctermbg=red        guifg=Magenta    guibg=Red      cterm=none         gui=none
   hi ansiCyanRed           ctermfg=cyan       ctermbg=red        guifg=Cyan       guibg=Red      cterm=none         gui=none
   hi ansiWhiteRed          ctermfg=white      ctermbg=red        guifg=White      guibg=Red      cterm=none         gui=none

   hi ansiBlackGreen        ctermfg=black      ctermbg=green      guifg=Black      guibg=Green    cterm=none         gui=none
   hi ansiRedGreen          ctermfg=red        ctermbg=green      guifg=Red        guibg=Green    cterm=none         gui=none
   hi ansiGreenGreen        ctermfg=green      ctermbg=green      guifg=Green      guibg=Green    cterm=none         gui=none
   hi ansiYellowGreen       ctermfg=yellow     ctermbg=green      guifg=Yellow     guibg=Green    cterm=none         gui=none
   hi ansiBlueGreen         ctermfg=blue       ctermbg=green      guifg=Blue       guibg=Green    cterm=none         gui=none
   hi ansiMagentaGreen      ctermfg=magenta    ctermbg=green      guifg=Magenta    guibg=Green    cterm=none         gui=none
   hi ansiCyanGreen         ctermfg=cyan       ctermbg=green      guifg=Cyan       guibg=Green    cterm=none         gui=none
   hi ansiWhiteGreen        ctermfg=white      ctermbg=green      guifg=White      guibg=Green    cterm=none         gui=none

   hi ansiBlackYellow       ctermfg=black      ctermbg=yellow     guifg=Black      guibg=Yellow   cterm=none         gui=none
   hi ansiRedYellow         ctermfg=red        ctermbg=yellow     guifg=Red        guibg=Yellow   cterm=none         gui=none
   hi ansiGreenYellow       ctermfg=green      ctermbg=yellow     guifg=Green      guibg=Yellow   cterm=none         gui=none
   hi ansiYellowYellow      ctermfg=yellow     ctermbg=yellow     guifg=Yellow     guibg=Yellow   cterm=none         gui=none
   hi ansiBlueYellow        ctermfg=blue       ctermbg=yellow     guifg=Blue       guibg=Yellow   cterm=none         gui=none
   hi ansiMagentaYellow     ctermfg=magenta    ctermbg=yellow     guifg=Magenta    guibg=Yellow   cterm=none         gui=none
   hi ansiCyanYellow        ctermfg=cyan       ctermbg=yellow     guifg=Cyan       guibg=Yellow   cterm=none         gui=none
   hi ansiWhiteYellow       ctermfg=white      ctermbg=yellow     guifg=White      guibg=Yellow   cterm=none         gui=none

   hi ansiBlackBlue         ctermfg=black      ctermbg=blue       guifg=Black      guibg=Blue     cterm=none         gui=none
   hi ansiRedBlue           ctermfg=red        ctermbg=blue       guifg=Red        guibg=Blue     cterm=none         gui=none
   hi ansiGreenBlue         ctermfg=green      ctermbg=blue       guifg=Green      guibg=Blue     cterm=none         gui=none
   hi ansiYellowBlue        ctermfg=yellow     ctermbg=blue       guifg=Yellow     guibg=Blue     cterm=none         gui=none
   hi ansiBlueBlue          ctermfg=blue       ctermbg=blue       guifg=Blue       guibg=Blue     cterm=none         gui=none
   hi ansiMagentaBlue       ctermfg=magenta    ctermbg=blue       guifg=Magenta    guibg=Blue     cterm=none         gui=none
   hi ansiCyanBlue          ctermfg=cyan       ctermbg=blue       guifg=Cyan       guibg=Blue     cterm=none         gui=none
   hi ansiWhiteBlue         ctermfg=white      ctermbg=blue       guifg=White      guibg=Blue     cterm=none         gui=none

   hi ansiBlackMagenta      ctermfg=black      ctermbg=magenta    guifg=Black      guibg=Magenta  cterm=none         gui=none
   hi ansiRedMagenta        ctermfg=red        ctermbg=magenta    guifg=Red        guibg=Magenta  cterm=none         gui=none
   hi ansiGreenMagenta      ctermfg=green      ctermbg=magenta    guifg=Green      guibg=Magenta  cterm=none         gui=none
   hi ansiYellowMagenta     ctermfg=yellow     ctermbg=magenta    guifg=Yellow     guibg=Magenta  cterm=none         gui=none
   hi ansiBlueMagenta       ctermfg=blue       ctermbg=magenta    guifg=Blue       guibg=Magenta  cterm=none         gui=none
   hi ansiMagentaMagenta    ctermfg=magenta    ctermbg=magenta    guifg=Magenta    guibg=Magenta  cterm=none         gui=none
   hi ansiCyanMagenta       ctermfg=cyan       ctermbg=magenta    guifg=Cyan       guibg=Magenta  cterm=none         gui=none
   hi ansiWhiteMagenta      ctermfg=white      ctermbg=magenta    guifg=White      guibg=Magenta  cterm=none         gui=none

   hi ansiBlackCyan         ctermfg=black      ctermbg=cyan       guifg=Black      guibg=Cyan     cterm=none         gui=none
   hi ansiRedCyan           ctermfg=red        ctermbg=cyan       guifg=Red        guibg=Cyan     cterm=none         gui=none
   hi ansiGreenCyan         ctermfg=green      ctermbg=cyan       guifg=Green      guibg=Cyan     cterm=none         gui=none
   hi ansiYellowCyan        ctermfg=yellow     ctermbg=cyan       guifg=Yellow     guibg=Cyan     cterm=none         gui=none
   hi ansiBlueCyan          ctermfg=blue       ctermbg=cyan       guifg=Blue       guibg=Cyan     cterm=none         gui=none
   hi ansiMagentaCyan       ctermfg=magenta    ctermbg=cyan       guifg=Magenta    guibg=Cyan     cterm=none         gui=none
   hi ansiCyanCyan          ctermfg=cyan       ctermbg=cyan       guifg=Cyan       guibg=Cyan     cterm=none         gui=none
   hi ansiWhiteCyan         ctermfg=white      ctermbg=cyan       guifg=White      guibg=Cyan     cterm=none         gui=none

   hi ansiBlackWhite        ctermfg=black      ctermbg=white      guifg=Black      guibg=White    cterm=none         gui=none
   hi ansiRedWhite          ctermfg=red        ctermbg=white      guifg=Red        guibg=White    cterm=none         gui=none
   hi ansiGreenWhite        ctermfg=green      ctermbg=white      guifg=Green      guibg=White    cterm=none         gui=none
   hi ansiYellowWhite       ctermfg=yellow     ctermbg=white      guifg=Yellow     guibg=White    cterm=none         gui=none
   hi ansiBlueWhite         ctermfg=blue       ctermbg=white      guifg=Blue       guibg=White    cterm=none         gui=none
   hi ansiMagentaWhite      ctermfg=magenta    ctermbg=white      guifg=Magenta    guibg=White    cterm=none         gui=none
   hi ansiCyanWhite         ctermfg=cyan       ctermbg=white      guifg=Cyan       guibg=White    cterm=none         gui=none
   hi ansiWhiteWhite        ctermfg=white      ctermbg=white      guifg=White      guibg=White    cterm=none         gui=none
  endif
"  call Dret("AnsiEsc#AnsiEsc")
endfun

" ---------------------------------------------------------------------
" s:MultiElementHandler: builds custom syntax highlighting for three or more element ansi escape sequences {{{2
fun! s:MultiElementHandler()
"  call Dfunc("s:MultiElementHandler()")
  let curwp= SaveWinPosn(0)
  keepj 1
  keepj norm! 0
  let mehcnt = 0
  let mehrules     = []
  while search('\e\[;\=\d\+;\d\+;\d\+\(;\d\+\)*m','cW')
   let curcol  = col(".")+1
   call search('m','cW')
   let mcol    = col(".")
   let ansiesc = strpart(getline("."),curcol,mcol - curcol)
   let aecodes = split(ansiesc,'[;m]')
"   call Decho("ansiesc<".ansiesc."> aecodes=".string(aecodes))
   let skip         = 0
   let mod          = "NONE,"
   let fg           = ""
   let bg           = ""

   " if the ansiesc is
   if index(mehrules,ansiesc) == -1
    let mehrules+= [ansiesc]

    for code in aecodes

     " handle multi-code sequences (38;5;color  and 48;5;color)
     if skip == 38 && code == 5
      " handling <esc>[38;5
      let skip= 385
"      call Decho(" 1: building code=".code." skip=".skip.": mod<".mod."> fg<".fg."> bg<".bg.">")
      continue
     elseif skip == 385
      " handling <esc>[38;5;...
      if has("gui") && has("gui_running")
       let fg= s:Ansi2Gui(code)
      else
       let fg= code
      endif
      let skip= 0
"      call Decho(" 2: building code=".code." skip=".skip.": mod<".mod."> fg<".fg."> bg<".bg.">")
      continue

     elseif skip == 48 && code == 5
      " handling <esc>[48;5
      let skip= 485
"      call Decho(" 3: building code=".code." skip=".skip.": mod<".mod."> fg<".fg."> bg<".bg.">")
      continue
     elseif skip == 485
      " handling <esc>[48;5;...
      if has("gui") && has("gui_running")
       let bg= s:Ansi2Gui(code)
      else
       let bg= code
      endif
      let skip= 0
"      call Decho(" 4: building code=".code." skip=".skip.": mod<".mod."> fg<".fg."> bg<".bg.">")
      continue

     else
      let skip= 0
     endif

     " handle single-code sequences
     if code == 1
      let mod=mod."bold,"
     elseif code == 2
      let mod=mod."italic,"
     elseif code == 3
      let mod=mod."standout,"
     elseif code == 4
      let mod=mod."underline,"
     elseif code == 5 || code == 6
      let mod=mod."undercurl,"
     elseif code == 7
      let mod=mod."reverse,"

     elseif code == 30
      let fg= "black"
     elseif code == 31
      let fg= "red"
     elseif code == 32
      let fg= "green"
     elseif code == 33
      let fg= "yellow"
     elseif code == 34
      let fg= "blue"
     elseif code == 35
      let fg= "magenta"
     elseif code == 36
      let fg= "cyan"
     elseif code == 37
      let fg= "white"

     elseif code == 40
      let bg= "black"
     elseif code == 41
      let bg= "red"
     elseif code == 42
      let bg= "green"
     elseif code == 43
      let bg= "yellow"
     elseif code == 44
      let bg= "blue"
     elseif code == 45
      let bg= "magenta"
     elseif code == 46
      let bg= "cyan"
     elseif code == 47
      let bg= "white"

     elseif code == 38
      let skip= 38

     elseif code == 48
      let skip= 48
     endif

"     call Decho(" 5: building code=".code." skip=".skip.": mod<".mod."> fg<".fg."> bg<".bg.">")
    endfor

    " fixups
    let mod= substitute(mod,',$','','')

    " build syntax-recognition rule
    let mehcnt  = mehcnt + 1
    let synrule = "syn region ansiMEH".mehcnt
    let synrule = synrule.' start="\e\['.ansiesc.'"'
    let synrule = synrule.' end="\e\["me=e-2'
    let synrule = synrule." contains=ansiConceal"
"    call Decho(" exe synrule: ".synrule)
    exe synrule

    " build highlighting rule
    let hirule= "hi ansiMEH".mehcnt
    if has("gui") && has("gui_running")
     let hirule=hirule." gui=".mod
     if fg != ""| let hirule=hirule." guifg=".fg| endif
     if bg != ""| let hirule=hirule." guibg=".bg| endif
    else
     let hirule=hirule." cterm=".mod
     if fg != ""| let hirule=hirule." ctermfg=".fg| endif
     if bg != ""| let hirule=hirule." ctermbg=".bg| endif
    endif
"    call Decho(" exe hirule: ".hirule)
    exe hirule
   endif

  endwhile

  call RestoreWinPosn(curwp)
"  call Dret("s:MultiElementHandler")
endfun

" ---------------------------------------------------------------------
" s:Ansi2Gui: converts an ansi-escape sequence (for 256-color xterms) {{{2
"           to an equivalent gui color
"           colors   0- 15:
"           colors  16-231:  6x6x6 color cube, code= 16+r*36+g*6+b  with r,g,b each in [0,5]
"           colors 232-255:  grayscale ramp,   code= 10*gray + 8    with gray in [0,23] (black,white left out)
fun! s:Ansi2Gui(code)
"  call Dfunc("s:Ansi2Gui(code=)".a:code)
  let guicolor= a:code
  if a:code < 16
   let code2rgb = [ "black", "red3", "green3", "yellow3", "blue3", "magenta3", "cyan3", "gray70", "gray40", "red", "green", "yellow", "royalblue3", "magenta", "cyan", "white"]
   let guicolor = code2rgb[a:code]
  elseif a:code >= 232
   let code     = a:code - 232
   let code     = 10*code + 8
   let guicolor = printf("#%02x%02x%02x",code,code,code)
  else
   let code     = a:code - 16
   let code2rgb = [43,85,128,170,213,255]
   let r        = code2rgb[code/36]
   let g        = code2rgb[(code%36)/6]
   let b        = code2rgb[code%6]
   let guicolor = printf("#%02x%02x%02x",r,g,b)
  endif
"  call Dret("s:Ansi2Gui ".guicolor)
  return guicolor
endfun

" ---------------------------------------------------------------------
"  Restore: {{{1
let &cpo= s:keepcpo
unlet s:keepcpo

" ---------------------------------------------------------------------
"  Modelines: {{{1
" vim: ts=12 fdm=marker
plugin/cecutil.vim	[[[1
536
" cecutil.vim : save/restore window position
"               save/restore mark position
"               save/restore selected user maps
"  Author:	Charles E. Campbell, Jr.
"  Version:	18h	ASTRO-ONLY
"  Date:	Apr 05, 2010
"
"  Saving Restoring Destroying Marks: {{{1
"       call SaveMark(markname)       let savemark= SaveMark(markname)
"       call RestoreMark(markname)    call RestoreMark(savemark)
"       call DestroyMark(markname)
"       commands: SM RM DM
"
"  Saving Restoring Destroying Window Position: {{{1
"       call SaveWinPosn()        let winposn= SaveWinPosn()
"       call RestoreWinPosn()     call RestoreWinPosn(winposn)
"		\swp : save current window/buffer's position
"		\rwp : restore current window/buffer's previous position
"       commands: SWP RWP
"
"  Saving And Restoring User Maps: {{{1
"       call SaveUserMaps(mapmode,maplead,mapchx,suffix)
"       call RestoreUserMaps(suffix)
"
" GetLatestVimScripts: 1066 1 :AutoInstall: cecutil.vim
"
" You believe that God is one. You do well. The demons also {{{1
" believe, and shudder. But do you want to know, vain man, that
" faith apart from works is dead?  (James 2:19,20 WEB)
"redraw!|call inputsave()|call input("Press <cr> to continue")|call inputrestore()

" ---------------------------------------------------------------------
" Load Once: {{{1
if &cp || exists("g:loaded_cecutil")
 finish
endif
let g:loaded_cecutil = "v18h"
let s:keepcpo        = &cpo
set cpo&vim
"DechoRemOn

" =======================
"  Public Interface: {{{1
" =======================

" ---------------------------------------------------------------------
"  Map Interface: {{{2
if !hasmapto('<Plug>SaveWinPosn')
 map <unique> <Leader>swp <Plug>SaveWinPosn
endif
if !hasmapto('<Plug>RestoreWinPosn')
 map <unique> <Leader>rwp <Plug>RestoreWinPosn
endif
nmap <silent> <Plug>SaveWinPosn		:call SaveWinPosn()<CR>
nmap <silent> <Plug>RestoreWinPosn	:call RestoreWinPosn()<CR>

" ---------------------------------------------------------------------
" Command Interface: {{{2
com! -bar -nargs=0 SWP	call SaveWinPosn()
com! -bar -nargs=? RWP	call RestoreWinPosn(<args>)
com! -bar -nargs=1 SM	call SaveMark(<q-args>)
com! -bar -nargs=1 RM	call RestoreMark(<q-args>)
com! -bar -nargs=1 DM	call DestroyMark(<q-args>)

com! -bar -nargs=1 WLR	call s:WinLineRestore(<q-args>)

if v:version < 630
 let s:modifier= "sil! "
else
 let s:modifier= "sil! keepj "
endif

" ===============
" Functions: {{{1
" ===============

" ---------------------------------------------------------------------
" SaveWinPosn: {{{2
"    let winposn= SaveWinPosn()  will save window position in winposn variable
"    call SaveWinPosn()          will save window position in b:cecutil_winposn{b:cecutil_iwinposn}
"    let winposn= SaveWinPosn(0) will *only* save window position in winposn variable (no stacking done)
fun! SaveWinPosn(...)
"  echomsg "Decho: SaveWinPosn() a:0=".a:0
  if line("$") == 1 && getline(1) == ""
"   echomsg "Decho: SaveWinPosn : empty buffer"
   return ""
  endif
  let so_keep   = &l:so
  let siso_keep = &siso
  let ss_keep   = &l:ss
  setlocal so=0 siso=0 ss=0

  let swline = line(".")                           " save-window line in file
  let swcol  = col(".")                            " save-window column in file
  if swcol >= col("$")
   let swcol= swcol + virtcol(".") - virtcol("$")  " adjust for virtual edit (cursor past end-of-line)
  endif
  let swwline   = winline() - 1                    " save-window window line
  let swwcol    = virtcol(".") - wincol()          " save-window window column
  let savedposn = ""
"  echomsg "Decho: sw[".swline.",".swcol."] sww[".swwline.",".swwcol."]"
  let savedposn = "call GoWinbufnr(".winbufnr(0).")"
  let savedposn = savedposn."|".s:modifier.swline
  let savedposn = savedposn."|".s:modifier."norm! 0z\<cr>"
  if swwline > 0
   let savedposn= savedposn.":".s:modifier."call s:WinLineRestore(".(swwline+1).")\<cr>"
  endif
  if swwcol > 0
   let savedposn= savedposn.":".s:modifier."norm! 0".swwcol."zl\<cr>"
  endif
  let savedposn = savedposn.":".s:modifier."call cursor(".swline.",".swcol.")\<cr>"

  " save window position in
  " b:cecutil_winposn_{iwinposn} (stack)
  " only when SaveWinPosn() is used
  if a:0 == 0
   if !exists("b:cecutil_iwinposn")
	let b:cecutil_iwinposn= 1
   else
	let b:cecutil_iwinposn= b:cecutil_iwinposn + 1
   endif
"   echomsg "Decho: saving posn to SWP stack"
   let b:cecutil_winposn{b:cecutil_iwinposn}= savedposn
  endif

  let &l:so = so_keep
  let &siso = siso_keep
  let &l:ss = ss_keep

"  if exists("b:cecutil_iwinposn")                                                                  " Decho
"   echomsg "Decho: b:cecutil_winpos{".b:cecutil_iwinposn."}[".b:cecutil_winposn{b:cecutil_iwinposn}."]"
"  else                                                                                             " Decho
"   echomsg "Decho: b:cecutil_iwinposn doesn't exist"
"  endif                                                                                            " Decho
"  echomsg "Decho: SaveWinPosn [".savedposn."]"
  return savedposn
endfun

" ---------------------------------------------------------------------
" RestoreWinPosn: {{{2
"      call RestoreWinPosn()
"      call RestoreWinPosn(winposn)
fun! RestoreWinPosn(...)
"  echomsg "Decho: RestoreWinPosn() a:0=".a:0
"  echomsg "Decho: getline(1)<".getline(1).">"
"  echomsg "Decho: line(.)=".line(".")
  if line("$") == 1 && getline(1) == ""
"   echomsg "Decho: RestoreWinPosn : empty buffer"
   return ""
  endif
  let so_keep   = &l:so
  let siso_keep = &l:siso
  let ss_keep   = &l:ss
  setlocal so=0 siso=0 ss=0

  if a:0 == 0 || a:1 == ""
   " use saved window position in b:cecutil_winposn{b:cecutil_iwinposn} if it exists
   if exists("b:cecutil_iwinposn") && exists("b:cecutil_winposn{b:cecutil_iwinposn}")
"    echomsg "Decho: using stack b:cecutil_winposn{".b:cecutil_iwinposn."}<".b:cecutil_winposn{b:cecutil_iwinposn}.">"
	try
	 exe s:modifier.b:cecutil_winposn{b:cecutil_iwinposn}
	catch /^Vim\%((\a\+)\)\=:E749/
	 " ignore empty buffer error messages
	endtry
	" normally drop top-of-stack by one
	" but while new top-of-stack doesn't exist
	" drop top-of-stack index by one again
	if b:cecutil_iwinposn >= 1
	 unlet b:cecutil_winposn{b:cecutil_iwinposn}
	 let b:cecutil_iwinposn= b:cecutil_iwinposn - 1
	 while b:cecutil_iwinposn >= 1 && !exists("b:cecutil_winposn{b:cecutil_iwinposn}")
	  let b:cecutil_iwinposn= b:cecutil_iwinposn - 1
	 endwhile
	 if b:cecutil_iwinposn < 1
	  unlet b:cecutil_iwinposn
	 endif
	endif
   else
	echohl WarningMsg
	echomsg "***warning*** need to SaveWinPosn first!"
	echohl None
   endif

  else	 " handle input argument
"   echomsg "Decho: using input a:1<".a:1.">"
   " use window position passed to this function
   exe a:1
   " remove a:1 pattern from b:cecutil_winposn{b:cecutil_iwinposn} stack
   if exists("b:cecutil_iwinposn")
	let jwinposn= b:cecutil_iwinposn
	while jwinposn >= 1                     " search for a:1 in iwinposn..1
	 if exists("b:cecutil_winposn{jwinposn}")    " if it exists
	  if a:1 == b:cecutil_winposn{jwinposn}      " and the pattern matches
	   unlet b:cecutil_winposn{jwinposn}            " unlet it
	   if jwinposn == b:cecutil_iwinposn            " if at top-of-stack
		let b:cecutil_iwinposn= b:cecutil_iwinposn - 1      " drop stacktop by one
	   endif
	  endif
	 endif
	 let jwinposn= jwinposn - 1
	endwhile
   endif
  endif

  " Seems to be something odd: vertical motions after RWP
  " cause jump to first column.  The following fixes that.
  " Note: was using wincol()>1, but with signs, a cursor
  " at column 1 yields wincol()==3.  Beeping ensued.
  let vekeep= &ve
  set ve=all
  if virtcol('.') > 1
   exe s:modifier."norm! hl"
  elseif virtcol(".") < virtcol("$")
   exe s:modifier."norm! lh"
  endif
  let &ve= vekeep

  let &l:so   = so_keep
  let &l:siso = siso_keep
  let &l:ss   = ss_keep

"  echomsg "Decho: RestoreWinPosn"
endfun

" ---------------------------------------------------------------------
" s:WinLineRestore: {{{2
fun! s:WinLineRestore(swwline)
"  echomsg "Decho: s:WinLineRestore(swwline=".a:swwline.")"
  while winline() < a:swwline
   let curwinline= winline()
   exe s:modifier."norm! \<c-y>"
   if curwinline == winline()
	break
   endif
  endwhile
"  echomsg "Decho: s:WinLineRestore"
endfun

" ---------------------------------------------------------------------
" GoWinbufnr: go to window holding given buffer (by number) {{{2
"   Prefers current window; if its buffer number doesn't match,
"   then will try from topleft to bottom right
fun! GoWinbufnr(bufnum)
"  call Dfunc("GoWinbufnr(".a:bufnum.")")
  if winbufnr(0) == a:bufnum
"   call Dret("GoWinbufnr : winbufnr(0)==a:bufnum")
   return
  endif
  winc t
  let first=1
  while winbufnr(0) != a:bufnum && (first || winnr() != 1)
  	winc w
	let first= 0
   endwhile
"  call Dret("GoWinbufnr")
endfun

" ---------------------------------------------------------------------
" SaveMark: sets up a string saving a mark position. {{{2
"           For example, SaveMark("a")
"           Also sets up a global variable, g:savemark_{markname}
fun! SaveMark(markname)
"  call Dfunc("SaveMark(markname<".a:markname.">)")
  let markname= a:markname
  if strpart(markname,0,1) !~ '\a'
   let markname= strpart(markname,1,1)
  endif
"  call Decho("markname=".markname)

  let lzkeep  = &lz
  set lz

  if 1 <= line("'".markname) && line("'".markname) <= line("$")
   let winposn               = SaveWinPosn(0)
   exe s:modifier."norm! `".markname
   let savemark              = SaveWinPosn(0)
   let g:savemark_{markname} = savemark
   let savemark              = markname.savemark
   call RestoreWinPosn(winposn)
  else
   let g:savemark_{markname} = ""
   let savemark              = ""
  endif

  let &lz= lzkeep

"  call Dret("SaveMark : savemark<".savemark.">")
  return savemark
endfun

" ---------------------------------------------------------------------
" RestoreMark: {{{2
"   call RestoreMark("a")  -or- call RestoreMark(savemark)
fun! RestoreMark(markname)
"  call Dfunc("RestoreMark(markname<".a:markname.">)")

  if strlen(a:markname) <= 0
"   call Dret("RestoreMark : no such mark")
   return
  endif
  let markname= strpart(a:markname,0,1)
  if markname !~ '\a'
   " handles 'a -> a styles
   let markname= strpart(a:markname,1,1)
  endif
"  call Decho("markname=".markname." strlen(a:markname)=".strlen(a:markname))

  let lzkeep  = &lz
  set lz
  let winposn = SaveWinPosn(0)

  if strlen(a:markname) <= 2
   if exists("g:savemark_{markname}") && strlen(g:savemark_{markname}) != 0
	" use global variable g:savemark_{markname}
"	call Decho("use savemark list")
	call RestoreWinPosn(g:savemark_{markname})
	exe "norm! m".markname
   endif
  else
   " markname is a savemark command (string)
"	call Decho("use savemark command")
   let markcmd= strpart(a:markname,1)
   call RestoreWinPosn(markcmd)
   exe "norm! m".markname
  endif

  call RestoreWinPosn(winposn)
  let &lz       = lzkeep

"  call Dret("RestoreMark")
endfun

" ---------------------------------------------------------------------
" DestroyMark: {{{2
"   call DestroyMark("a")  -- destroys mark
fun! DestroyMark(markname)
"  call Dfunc("DestroyMark(markname<".a:markname.">)")

  " save options and set to standard values
  let reportkeep= &report
  let lzkeep    = &lz
  set lz report=10000

  let markname= strpart(a:markname,0,1)
  if markname !~ '\a'
   " handles 'a -> a styles
   let markname= strpart(a:markname,1,1)
  endif
"  call Decho("markname=".markname)

  let curmod  = &mod
  let winposn = SaveWinPosn(0)
  1
  let lineone = getline(".")
  exe "k".markname
  d
  put! =lineone
  let &mod    = curmod
  call RestoreWinPosn(winposn)

  " restore options to user settings
  let &report = reportkeep
  let &lz     = lzkeep

"  call Dret("DestroyMark")
endfun

" ---------------------------------------------------------------------
" QArgSplitter: to avoid \ processing by <f-args>, <q-args> is needed. {{{2
" However, <q-args> doesn't split at all, so this one returns a list
" with splits at all whitespace (only!), plus a leading length-of-list.
" The resulting list:  qarglist[0] corresponds to a:0
"                      qarglist[i] corresponds to a:{i}
fun! QArgSplitter(qarg)
"  call Dfunc("QArgSplitter(qarg<".a:qarg.">)")
  let qarglist    = split(a:qarg)
  let qarglistlen = len(qarglist)
  let qarglist    = insert(qarglist,qarglistlen)
"  call Dret("QArgSplitter ".string(qarglist))
  return qarglist
endfun

" ---------------------------------------------------------------------
" ListWinPosn: {{{2
"fun! ListWinPosn()                                                        " Decho 
"  if !exists("b:cecutil_iwinposn") || b:cecutil_iwinposn == 0             " Decho 
"   call Decho("nothing on SWP stack")                                     " Decho
"  else                                                                    " Decho
"   let jwinposn= b:cecutil_iwinposn                                       " Decho 
"   while jwinposn >= 1                                                    " Decho 
"    if exists("b:cecutil_winposn{jwinposn}")                              " Decho 
"     call Decho("winposn{".jwinposn."}<".b:cecutil_winposn{jwinposn}.">") " Decho 
"    else                                                                  " Decho 
"     call Decho("winposn{".jwinposn."} -- doesn't exist")                 " Decho 
"    endif                                                                 " Decho 
"    let jwinposn= jwinposn - 1                                            " Decho 
"   endwhile                                                               " Decho 
"  endif                                                                   " Decho
"endfun                                                                    " Decho 
"com! -nargs=0 LWP	call ListWinPosn()                                    " Decho 

" ---------------------------------------------------------------------
" SaveUserMaps: this function sets up a script-variable (s:restoremap) {{{2
"          which can be used to restore user maps later with
"          call RestoreUserMaps()
"
"          mapmode - see :help maparg for details (n v o i c l "")
"                    ex. "n" = Normal
"                    The letters "b" and "u" are optional prefixes;
"                    The "u" means that the map will also be unmapped
"                    The "b" means that the map has a <buffer> qualifier
"                    ex. "un"  = Normal + unmapping
"                    ex. "bn"  = Normal + <buffer>
"                    ex. "bun" = Normal + <buffer> + unmapping
"                    ex. "ubn" = Normal + <buffer> + unmapping
"          maplead - see mapchx
"          mapchx  - "<something>" handled as a single map item.
"                    ex. "<left>"
"                  - "string" a string of single letters which are actually
"                    multiple two-letter maps (using the maplead:
"                    maplead . each_character_in_string)
"                    ex. maplead="\" and mapchx="abc" saves user mappings for
"                        \a, \b, and \c
"                    Of course, if maplead is "", then for mapchx="abc",
"                    mappings for a, b, and c are saved.
"                  - :something  handled as a single map item, w/o the ":"
"                    ex.  mapchx= ":abc" will save a mapping for "abc"
"          suffix  - a string unique to your plugin
"                    ex.  suffix= "DrawIt"
fun! SaveUserMaps(mapmode,maplead,mapchx,suffix)
"  call Dfunc("SaveUserMaps(mapmode<".a:mapmode."> maplead<".a:maplead."> mapchx<".a:mapchx."> suffix<".a:suffix.">)")

  if !exists("s:restoremap_{a:suffix}")
   " initialize restoremap_suffix to null string
   let s:restoremap_{a:suffix}= ""
  endif

  " set up dounmap: if 1, then save and unmap  (a:mapmode leads with a "u")
  "                 if 0, save only
  let mapmode  = a:mapmode
  let dounmap  = 0
  let dobuffer = ""
  while mapmode =~ '^[bu]'
   if     mapmode =~ '^u'
    let dounmap = 1
    let mapmode = strpart(a:mapmode,1)
   elseif mapmode =~ '^b'
    let dobuffer = "<buffer> "
    let mapmode  = strpart(a:mapmode,1)
   endif
  endwhile
"  call Decho("dounmap=".dounmap."  dobuffer<".dobuffer.">")
 
  " save single map :...something...
  if strpart(a:mapchx,0,1) == ':'
"   call Decho("save single map :...something...")
   let amap= strpart(a:mapchx,1)
   if amap == "|" || amap == "\<c-v>"
    let amap= "\<c-v>".amap
   endif
   let amap                    = a:maplead.amap
   let s:restoremap_{a:suffix} = s:restoremap_{a:suffix}."|:silent! ".mapmode."unmap ".dobuffer.amap
   if maparg(amap,mapmode) != ""
    let maprhs                  = substitute(maparg(amap,mapmode),'|','<bar>','ge')
	let s:restoremap_{a:suffix} = s:restoremap_{a:suffix}."|:".mapmode."map ".dobuffer.amap." ".maprhs
   endif
   if dounmap
	exe "silent! ".mapmode."unmap ".dobuffer.amap
   endif
 
  " save single map <something>
  elseif strpart(a:mapchx,0,1) == '<'
"   call Decho("save single map <something>")
   let amap       = a:mapchx
   if amap == "|" || amap == "\<c-v>"
    let amap= "\<c-v>".amap
"	call Decho("amap[[".amap."]]")
   endif
   let s:restoremap_{a:suffix} = s:restoremap_{a:suffix}."|silent! ".mapmode."unmap ".dobuffer.amap
   if maparg(a:mapchx,mapmode) != ""
    let maprhs                  = substitute(maparg(amap,mapmode),'|','<bar>','ge')
	let s:restoremap_{a:suffix} = s:restoremap_{a:suffix}."|".mapmode."map ".dobuffer.amap." ".maprhs
   endif
   if dounmap
	exe "silent! ".mapmode."unmap ".dobuffer.amap
   endif
 
  " save multiple maps
  else
"   call Decho("save multiple maps")
   let i= 1
   while i <= strlen(a:mapchx)
    let amap= a:maplead.strpart(a:mapchx,i-1,1)
	if amap == "|" || amap == "\<c-v>"
	 let amap= "\<c-v>".amap
	endif
	let s:restoremap_{a:suffix} = s:restoremap_{a:suffix}."|silent! ".mapmode."unmap ".dobuffer.amap
    if maparg(amap,mapmode) != ""
     let maprhs                  = substitute(maparg(amap,mapmode),'|','<bar>','ge')
	 let s:restoremap_{a:suffix} = s:restoremap_{a:suffix}."|".mapmode."map ".dobuffer.amap." ".maprhs
    endif
	if dounmap
	 exe "silent! ".mapmode."unmap ".dobuffer.amap
	endif
    let i= i + 1
   endwhile
  endif
"  call Dret("SaveUserMaps : restoremap_".a:suffix.": ".s:restoremap_{a:suffix})
endfun

" ---------------------------------------------------------------------
" RestoreUserMaps: {{{2
"   Used to restore user maps saved by SaveUserMaps()
fun! RestoreUserMaps(suffix)
"  call Dfunc("RestoreUserMaps(suffix<".a:suffix.">)")
  if exists("s:restoremap_{a:suffix}")
   let s:restoremap_{a:suffix}= substitute(s:restoremap_{a:suffix},'|\s*$','','e')
   if s:restoremap_{a:suffix} != ""
"   	call Decho("exe ".s:restoremap_{a:suffix})
    exe "silent! ".s:restoremap_{a:suffix}
   endif
   unlet s:restoremap_{a:suffix}
  endif
"  call Dret("RestoreUserMaps")
endfun

" ==============
"  Restore: {{{1
" ==============
let &cpo= s:keepcpo
unlet s:keepcpo

" ================
"  Modelines: {{{1
" ================
" vim: ts=4 fdm=marker
doc/AnsiEsc.txt	[[[1
134
*AnsiEsc.txt*	Ansi Escape Sequence Visualization		Dec 13, 2010

Author:  Charles E. Campbell, Jr.  <NdrOchip@ScampbellPfamily.AbizM>
	  (remove NOSPAM from Campbell's email first)
Copyright: (c) 2004-2010 by Charles E. Campbell, Jr.	*AnsiEsc-copyright*
           The VIM LICENSE applies to AnsiEsc.vim and AnsiEsc.txt
           (see |copyright|) except use "AnsiEsc" instead of "Vim".
	   No warranty, express or implied.  Use At-Your-Own-Risk.

==============================================================================
1. Contents					*AnsiEsc* *AnsiEsc-contents*
   1. Contents         ...................................|AnsiEsc-contents|
   2. AnsiEsc Manual   ...................................|AnsiEsc|
   3. AnsiEsc History  ...................................|AnsiEsc-history|

==============================================================================
2. Manual						*AnsiEsc-manual*

	CONCEAL -- the best mode.
		For this, your vim must have +conceal.  A typical way to get the
		conceal feature:

		* cd ..wherever../vim72/
		* wget http://vince.negri.googlepages.com/conceal-ownsyntax.diff
		* patch -p0 < conceal-ownsyntax.diff
		* make distclean
		* configure --with-features=huge
		* make
		* make install
	
	Normal: -- ansi escape sequences themselves are Ignore'd~
		Ansi escape sequences have the expected effect on subsequent
		text, but the ansi escape sequences themselves still take up
		screen columns.  The sequences are displayed using "Ignore"
		highlighting; depending on your colorscheme, this should either
		make the sequences blend into your background or be visually
		suppressed.  If the sequences aren't suppressed, you need to
		improve your colorscheme!

	USAGE~
		:AnsiEsc   -- toggles Ansi escape sequence highlighting
		:AnsiEsc!  -- rebuilds highlighting for new/removed three
		              or more element Ansi escape sequences.
	
	RESULT~
		Ansi escape sequences become concealed or ignored (depending
		on whether your vim supports Negri's conceal mode), and their
		effect on subsequent text is emulated with Vim's syntax
		highlighting.

		Syntax highlighting for one and two element codes are
		hard-coded into AnsiEsc.vim.  There are too many possibilities
		for three or more element codes; these are supported by
		examining the file for such sequences and only building syntax
		highlighting rules for such sequences as are actually present
		in the document.

	EXAMPLE~

		You'll want to use   :AnsiEsc   to see the following properly!
		(using Vince Negri's conceal option or vim 7.3)

            [34;47mColor Escape Sequences[m
[37m  -  [m   [37;1m  1  [m   [37;2m  2  [m   [37;3m  3  [m   [37;4m  4  [m   [37;5m  5  [m   [37;7m  7  [m
[30mblack[m   [30;1mblack[m   [30;2mblack[m   [30;3mblack[m   [30;4mblack[m   [30;5mblack[m   [30;7mblack[m
[31mred[m     [31;1mred[m     [31;2mred[m     [31;3mred[m     [31;4mred[m     [31;5mred[m     [31;7mred[m
[32mgreen[m   [32;1mgreen[m   [32;2mgreen[m   [32;3mgreen[m   [32;4mgreen[m   [32;5mgreen[m   [32;7mgreen[m
[33myellow[m  [33;1myellow[m  [33;2myellow[m  [33;3myellow[m  [33;4myellow[m  [33;5myellow[m  [33;7myellow[m
[34mblue[m    [34;1mblue[m    [34;2mblue[m    [34;3mblue[m    [34;4mblue[m    [34;5mblue[m    [34;7mblue[m
[35mmagenta[m [35;1mmagenta[m [35;2mmagenta[m [35;3mmagenta[m [35;4mmagenta[m [35;5mmagenta[m [35;7mmagenta[m
[36mcyan[m    [36;1mcyan[m    [36;2mcyan[m    [36;3mcyan[m    [36;4mcyan[m    [36;5mcyan[m    [36;7mcyan[m
[37mwhite[m   [37;1mwhite[m   [37;2mwhite[m   [37;3mwhite[m   [37;4mwhite[m   [37;5mwhite[m   [37;7mwhite[m

Black   [30;40mB[m  [30;41mB[m  [30;42mB[m  [30;43mB[m  [30;44mB[m   [30;45mB[m   [30;46mB[m   [30;47mB[m
Red     [31;40mR[m  [31;41mR[m  [31;42mR[m  [31;43mR[m  [31;44mR[m   [31;45mR[m   [31;46mR[m   [31;47mR[m
Green   [32;40mG[m  [32;41mG[m  [32;42mG[m  [32;43mG[m  [32;44mG[m   [32;45mG[m   [32;46mG[m   [32;47mG[m
Yellow  [33;40mY[m  [33;41mY[m  [33;42mY[m  [33;43mY[m  [33;44mY[m   [33;45mY[m   [33;46mY[m   [33;47mY[m
Blue    [34;40mB[m  [34;41mB[m  [34;42mB[m  [34;43mB[m  [34;44mB[m   [34;45mB[m   [34;46mB[m   [34;47mB[m
Magenta [35;40mM[m  [35;41mM[m  [35;42mM[m  [35;43mM[m  [35;44mM[m   [35;45mM[m   [35;46mM[m   [35;47mM[m
Cyan    [36;40mC[m  [36;41mC[m  [36;42mC[m  [36;43mC[m  [36;44mC[m   [36;45mC[m   [36;46mC[m   [36;47mC[m
White   [37;40mW[m  [37;41mW[m  [37;42mW[m  [37;43mW[m  [37;44mW[m   [37;45mW[m   [37;46mW[m   [37;47mW[m

	Here's the vim logo:

        [30;48;5;22m/  \[m
       [30;48;5;22m/    \[m
      [30;48;5;22m/      \[m
     [30;48;5;22m/        \[m
 [38;5;34;48;5;251m+----+[30;48;5;22m [38;5;34;48;5;251m+----+[30;48;5;22m \[m
 [38;5;34;48;5;251m++  ++[30;48;5;22m [38;5;34;48;5;251m+-   |[30;48;5;22m  \[m
 [30;48;5;22m/[38;5;34;48;5;251m|  |[30;48;5;22m   [m[38;5;34;48;5;251m/  /[30;48;5;22m    \[m
[30;48;5;22mX [38;5;34;48;5;251m|  |[30;48;5;22m  [38;5;34;48;5;251m/  /O[30;48;5;22m     \[m
 [30;48;5;22m\[38;5;34;48;5;251m|  |[30;48;5;22m [38;5;34;48;5;251m/  /+-+[30;48;5;22m [38;5;34;48;5;251m+-\[30;48;5;22m/[38;5;34;48;5;251m/-+[m
  [38;5;34;48;5;251m|  |/  /[30;48;5;22m [38;5;34;48;5;251m| |[30;48;5;22m [38;5;34;48;5;251m|  v  |[m
  [38;5;34;48;5;251m|  /  /[30;48;5;22m  [38;5;34;48;5;251m| |[30;48;5;22m [m[38;5;34;48;5;251m| +  [38;5;34;48;5;251m+|[m
  [38;5;34;48;5;251m|    /[30;48;5;22m   [38;5;34;48;5;251m| |[30;48;5;22m/[38;5;34;48;5;251m| |[38;5;34;48;5;251m\/[m[38;5;34;48;5;251m||[m
  [38;5;34;48;5;251m+----[30;48;5;22m\   [38;5;34;48;5;251m+-+ [38;5;34;48;5;251m+-+[m  [38;5;34;48;5;251m++[m
        [30;48;5;22m\   /[m
         [30;48;5;22m\ /[m


==============================================================================
3. AnsiEsc History					*AnsiEsc-history* {{{1
  v12	Jul 23, 2010	* changed conc to |'cole'| to correspond to vim 7.3's
			  change
	Aug 10, 2010	* (Rainer M Schmid) changed conceallevel setting to
			  depend on whether the version is before vim 7.3;
			  for 7.3, also sets concealcursor
			* Restores conc/cole/cocu settings when AnsiEsc is
			  toggled off.
	Dec 13, 2010	* Included some additional sequences involving 0
  v11	Apr 20, 2010	* AnsiEsc now supports enabling/disabling via a menu
			* <esc>[K and <esc>[00m now supported (as
			  grep --color=always   issues them)
  v10   May 06, 2009	* Three or more codes in an ANSI escape sequence are
			  supported by building custom syntax and highlighting
			  commands.
	May 20, 2009	* cecutil bugfix
  v9    May 12, 2008    * Now in plugin + autoload format.  Provides :AnsiEsc
                          command to toggle Ansi-escape sequence processing.
	Jan 01, 2009	* Applies Ignore highlighting to extended Ansi escape
			  sequences support 256-colors.
	Mar 18, 2009    * Includes "rapid blink" ansi escape sequences.  Vim
			  doesn't have a blinking attribute, so such text uses
			  "standout" for vim and "undercurl" for gvim.
  v8	Aug 16, 2006	* Uses undercurl, and so is only available for vim 7.0
  v7  	Dec 14, 2004	* Works better with vim2ansi output and Vince Negri's
			  conceal patch for vim 6.x.
  v2	Nov 24, 2004	* This version didn't use Vince Negri's conceal patch
			  (used Ignore highlighting)

==============================================================================
Modelines: {{{1
vim:tw=78:ts=8:ft=help:fdm=marker:
