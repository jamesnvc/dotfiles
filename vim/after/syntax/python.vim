if !has('conceal')
  !finish
endif

syntax clear pythonOperator

syntax keyword pythonOperator is

syntax match pyNiceOperator "\<in\>" conceal cchar=∈
" include the space after “not” – if present – so that “not a” becomes “¬a”
syntax match pyNiceOperator "\<not\%( \|\>\)" conceal cchar=¬
syntax match pyNiceOperator "\<not in\>" conceal cchar=∉
syntax match pyNiceOperator "\<or\>" conceal cchar=∨
syntax match pyNiceOperator "\<and\>" conceal cchar=∧
syntax match pyNiceOperator "<=" conceal cchar=≤
syntax match pyNiceOperator ">=" conceal cchar=≥
" only conceal “==” if alone, to avoid concealing SCM conflict markers
syntax match pyNiceOperator "=\@<!===\@!" conceal cchar=≡
syntax match pyNiceOperator "!=" conceal cchar=≠

syntax keyword pyNiceOperator sum conceal cchar=∑
syntax match pyNiceOperator "\<\%(math\.\)\?sqrt\>" conceal cchar=√
syntax match pyNiceKeyword "\<\%(math\.\)\?pi\>" conceal cchar=π

syntax keyword pyNiceStatement lambda conceal cchar=λ

syn match pyNiceVariable '\a\@<!Delta\%(\a\)\@!' conceal cchar=Δ containedin=pythonFunction
syn match pyNiceVariable '\a\@<!Lambda\%(\a\)\@!' conceal cchar=Λ containedin=pythonFunction
syn match pyNiceVariable '\a\@<!alpha\%(\a\)\@!' conceal cchar=α containedin=pythonFunction
syn match pyNiceVariable '\a\@<!beta\%(\a\)\@!' conceal cchar=β containedin=pythonFunction
syn match pyNiceVariable '\a\@<!delta\%(\a\)\@!' conceal cchar=δ containedin=pythonFunction
syn match pyNiceVariable '\a\@<!epsilon\%(\a\)\@!' conceal cchar=ε containedin=pythonFunction
syn match pyNiceVariable '\a\@<!eta\%(\a\)\@!' conceal cchar=η containedin=pythonFunction
syn match pyNiceVariable '\a\@<!mu\%(\a\)\@!' conceal cchar=μ containedin=pythonFunction
syn match pyNiceVariable '\a\@<!nabla\%(\a\)\@!' conceal cchar=∇ containedin=pythonFunction
syn match pyNiceVariable '\a\@<!sigma\%(\a\)\@!' conceal cchar=σ containedin=pythonFunction
syn match pyNiceVariable '\a\@<!theta\%(\a\)\@!' conceal cchar=θ containedin=pythonFunction

hi link pyNiceOperator Operator
hi link pyNiceStatement Statement
hi link pyNiceKeyword Keyword
hi! link Conceal Operator

setlocal conceallevel=2

syn keyword pyIWantHighlight True False None self
hi link pyIWantHighlight Keyword
