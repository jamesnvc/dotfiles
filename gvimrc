" Disable command-t as newtab, so we can use CommandT plugin
if has("gui_macvim")
  macmenu &File.New\ Tab key=<nop>
end
