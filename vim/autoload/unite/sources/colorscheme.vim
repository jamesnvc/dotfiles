let s:unite_source = {
      \ 'name': 'colorscheme',
      \ }

function! s:unite_source.gather_candidates(args, context)
  " [(name, dir)]
  " e.g. [('adaryn', '/Users/ujihisa/.vimbundles/ColorSamplerPack/colors'), ...]
  let colorlist = map(split(globpath(&runtimepath, 'colors/*.vim'), '\n'),
        \'[fnamemodify(v:val, ":t:r"), fnamemodify(v:val, ":h")]')

  return map(colorlist, '{
        \ "word": v:val[0],
        \ "source": "colorscheme",
        \ "kind": "colorscheme",
        \ "action__path": printf("%s/%s.vim", v:val[1], v:val[0]),
        \ "action__directory": v:val[1],
        \ }')
endfunction

function! unite#sources#colorscheme#define()
  return s:unite_source
endfunction
