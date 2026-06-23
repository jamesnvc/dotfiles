source "$HOME/.config/emacs/straight/repos/ghostel/etc/shell/ghostel.fish"

function e --description "Open file in emacs"
    ghostel_cmd find-file (realpath $argv)
end

function d --description "Open directory in emacs"
    ghostel_cmd dired (realpath $argv)
end

function gst --description "Open magit in directory"
    ghostel_cmd magit-status (pwd)
end
