" A Vim plugin that defines a parameter text object.
" Maintainer: David Larson <david@thesilverstream.com>
" Last Change: Mar 15, 2010
"
" This script defines a parameter text object. A parameter is the text between
" parentheses or commas, typically found in a function's argument list.
"
" See:
" :help text-objects
"   for a description of what can be done with text objects.
" Also See:
" :help a(
"   If you want to operate on the parentheses also.
"
" Like all the other text-objects, a parameter text object can be selected
" following these commands: 'd', 'c', 'y', 'v', etc. The script defines these
" operator mappings:
"
"    aP    "a parameter", select a parameter, including one comma (if there is
"          one).
"
"    iP    "inner parameter", select a parameter, not including commas.
" 
" If you would like to remap the commands then you can prevent the default
" mappings from getting set if you set g:no_parameter_object_maps = 1 in your
" .vimrc file. Then remap the commands as desired, like this:
"
"    let g:no_parameter_object_maps = 1
"    vmap     <silent> ia <Plug>ParameterObjectI
"    omap     <silent> ia <Plug>ParameterObjectI
"    vmap     <silent> aa <Plug>ParameterObjectA
"    omap     <silent> aa <Plug>ParameterObjectA

if exists("loaded_parameter_objects") || &cp || v:version < 701
  finish
endif
let loaded_parameter_objects = 1

if !exists("g:no_parameter_object_maps") || !g:no_parameter_object_maps
   vmap     <silent> iP <Plug>ParameterObjectI
   omap     <silent> iP <Plug>ParameterObjectI
   vmap     <silent> aP <Plug>ParameterObjectA
   omap     <silent> aP <Plug>ParameterObjectA
endif
vnoremap <silent> <script> <Plug>ParameterObjectI :<C-U>call <SID>parameter_object("i")<cr>
onoremap <silent> <script> <Plug>ParameterObjectI :call <SID>parameter_object("i")<cr>
vnoremap <silent> <script> <Plug>ParameterObjectA :<C-U>call <SID>parameter_object("a")<cr>
onoremap <silent> <script> <Plug>ParameterObjectA :call <SID>parameter_object("a")<cr>

function s:parameter_object(mode)
   let ve_save = &ve
   set virtualedit=onemore
   let l_save = @l
   try
      " Search for the start of the parameter text object
      if searchpair('(',',',')', 'bWs', "s:skip()") <= 0
         return
      endif

      normal! "lyl
      if a:mode == "a" && @l == ','
         let gotone = 1
         normal! ml
      else
         normal! lmlh
      endif

      let c = v:count1
      while c
         " Search for the end of the parameter text object
         if searchpair('(',',',')', 'W', "s:skip()") <= 0
            normal! `'
            return
         endif
         normal! "lyl
         if @l == ')' && c > 1
            " found the last parameter when more is asked for, so abort
            normal! `'
            return
         endif
         let c -= 1
      endwhile

      if a:mode == "a" && @l == ',' && !exists("gotone")
      else
         normal! h
      endif
      normal! v`l
   finally
      let &ve = ve_save
      let @l = l_save
   endtry
endfunction
function s:skip()
   let name = synIDattr(synID(line("."), col("."), 0), "name")
   if name =~? "comment"
      return 1
   elseif name =~? "string"
      return 1
   endif
   return 0
endfunction

" vim:fdm=marker fmr=function\ ,endfunction
