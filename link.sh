#!/usr/bin/env bash

HERE="$(pwd)"

link() {
    ln -s "${HERE}/$1" "$2"
}

link alacritty ~/.config/alacritty
link aliases ~/.aliases
link emacs.d ~/.emacs.d
link exports ~/.exports
link fish ~/.config/fish
link gitconfig ~/.gitconfig
link git-util/* ~/bin
link gtk-3.0 ~/.config/gtk-3.0
link i3 ~/.i3
link i3blocks.conf ~/.i3blocks.conf
link i3status.conf ~/.i3status.conf
link lein_profiles.clj ~/.lein/profiles.clj
link offlineimap ~/.config/offlineimap
link psqlrc ~/.psqlrc
link rofi ~/.config/rofi
link rofi-pass ~/.config/rofi-pass
link rofi_runner.sh ~/bin/rofi_runner.sh
link ssh ~/.ssh
link tmux ~/.tmux
link tmux.conf ~/.tmux.conf
link tridactyl ~/.config/tridactyl
link web-search.sh ~/bin/web-search.sh
link Xresources ~/.Xresources
link xsessionrc ~/.xsessionrc
link zsh.d ~/.zsh.d
link zshenv ~/.zshenv
link zshrc ~/.zshrc
link mbsyncrc ~/.mbsyncrc
link notmuch-config ~/.notmuch-config
# To set up systemd user units,
# systemctl --user daemon-reload
# systemctl --user start mbsync.timer
link systemd_user/mbsync.service ~/.config/systemd/user/mbsync.service
link systemd_user/mbsync.timer ~/.config/systemd/user/mbsync.timer
link notmuch_hooks ~/.mail/gmail/.notmuch/hooks
