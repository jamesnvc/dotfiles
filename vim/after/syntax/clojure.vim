syntax keyword lispFunc defpartial when-not defproject defpage

syntax match lispKey '\<:[^ )]\+\>'

syn region lispList     matchgroup=Delimiter start="\["   skip="|.\{-}|" matchgroup=Delimiter end="]"  contains=@lispListCluster
syn region lispBQList     matchgroup=PreProc   start="`\["  skip="|.\{-}|" matchgroup=PreProc   end="]"    contains=@lispListCluster

syn region lispList matchgroup=Delimiter start="{" skip="|.\{-}|" matchgroup=Delimiter end="}" contains=@lispListCluster

