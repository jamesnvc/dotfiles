" Disable command-t as newtab, so we can use CommandT plugin
if has("gui_macvim")
  macmenu &File.New\ Tab key=<nop>
  map <D-t> <Plug>PeepOpen
end
set guioptions-=T
set guioptions-=m
set guioptions-=rL
set guicursor=a:blinkon0
set guifont=PragmataPro:h10
if filereadable(expand("~/.gvimrc.local"))
  source ~/.gvimrc.local
endif
