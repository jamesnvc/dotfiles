# Un-defining to get rid of the vim insert mode thing
function fish_default_mode_prompt
end

fish_vi_key_bindings

set -gx EDITOR 'emacsclient --create-frame --alternate-editor=emacs'

alias g=git
alias l=ls
alias ec="emacsclient --create-frame --alternate-editor=emacs"
