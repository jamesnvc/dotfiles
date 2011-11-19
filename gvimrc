" Disable command-t as newtab, so we can use CommandT plugin
if has("gui_macvim")
  macmenu &File.New\ Tab key=<nop>
end
set guioptions-=T
set guioptions-=m
set guioptions-=rL
set guicursor=a:blinkon0
set guifont=Inconsolata
