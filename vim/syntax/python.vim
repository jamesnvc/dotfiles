setl foldmethod=syntax
syn sync fromstart
syn region pythonFunctionFold start="^\z(\s*\)\%(def\|class\) " skip="^\s*$" end="\ze\n\%(\z1\s\)\@!" fold transparent

