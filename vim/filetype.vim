if exists("did_load_filetypes")
  finish
endif

augroup filetypedetect
  autocmd!
  autocmd BufRead,BufNewFile *.json setfiletype javascript
  autocmd BufRead,BufNewFile *.ru   setfiletype ruby
  autocmd BufRead,BufNewFile *.mu   setfiletype mustache
  autocmd BufRead,BufNewFile *.m    setfiletype objc
augroup END
