# Un-defining to get rid of the vim insert mode thing
function fish_default_mode_prompt
end

set -gx EDITOR 'emacsclient --create-frame --alternate-editor=emacs'
# overriding weird ubuntu thing
if test ( uname ) = 'Linux'
    set -gx LD_PRELOAD /usr/lib/x86_64-linux-gnu/libgtk3-nocsd.so.0
end
# set -U fish_key_bindings fish_vi_key_bindings

alias g=git
alias l=ls
alias ec="emacsclient --create-frame --alternate-editor=emacs"

direnv hook fish | source
