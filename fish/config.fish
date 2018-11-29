set -U fish_key_bindings fish_vi_key_bindings
set -g -x EDITOR "emacsclient --create-frame --alternate-editor=emacs"

function ec
    emacsclient --create-frame --alternate-editor=emacs
end

# Un-defining to get rid of the vim insert mode thing
function fish_default_mode_prompt
end
