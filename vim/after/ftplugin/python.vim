match ErrorMsg '\%>80v.\+'

" Add the virtualenv's site-packages to vim path
py << EOF
import os
import sys
import vim
try:
  project_base_dir = os.environ['VIRTUAL_ENV']
  sys.path.insert(0, project_base_dir)
  activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
  execfile(activate_this, dict(__file__=activate_this))
except KeyError:
  for p in sys.path:
    if os.path.isdir(p):
      vim.command(r"set path+=%s" % (p.replace(" ", r"\ ")))
EOF

python << EOL
import vim, re
def EvaluateCurrentRange():
  # Copy the range so we don't modify the buffer
  lines = list(vim.current.range)
  # Trim leading whitespace
  initialWhitespace =  re.match(r'^\s*', vim.current.range[0]).group(0)
  if initialWhitespace:
    wsLen = len(initialWhitespace)
    for i, line in enumerate(lines):
      lines[i] = line[wsLen:]
  try:
    eval(compile('\n'.join(lines), '', 'exec'), globals())
  except Exception:
    print "Error evaluating region"
EOL
if has('mac')
  vmap <D-r> :python EvaluateCurrentRange()<CR>
else
  vmap <C-S-r> :python EvaluateCurrentRange()<CR>
endif
