# Un-defining to get rid of the vim insert mode thing
function fish_default_mode_prompt
end

set -gx EDITOR 'emacsclient --create-frame --alternate-editor=emacs'
# overriding weird ubuntu thing
if test ( uname ) = 'Linux'
    set -gx LD_PRELOAD /usr/lib/x86_64-linux-gnu/libgtk3-nocsd.so.0
end
# set -U fish_key_bindings fish_vi_key_bindings

set -g XDG_DATA_DIRS $HOME/share:$XDG_DATA_DIRS

alias g=git
alias l=ls
alias ec="emacsclient --create-frame --alternate-editor=emacs"

if test -d "$HOME/src/plugin-foreign-env"
    set fish_function_path $fish_function_path "$HOME/src/plugin-foreign-env/functions"
end

if is_func fenv
    if test -f /etc/profile.d/nix.sh
        fenv source /etc/profile.d/nix.sh
    else if test -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
        fenv source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
    else if test -f "$HOME/.nix-profile/etc/profile.d/nix.sh"
        fenv source "$HOME/.nix-profile/etc/profile.d/nix.sh"
    end
end

direnv hook fish | source
pyenv init - | source
