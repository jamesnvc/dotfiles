# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall

# local _myhosts
# _myhosts=( ${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
# zstyle ':completion:*' hosts $_myhosts

# eval "`dircolors -b`"

if [ TERM="xterm" ]; then
    export TERM='xterm-color'
fi

# what progs can use the hostname completion
compctl -k hostnames ping telnet ftp nslookup ssh traceroute mtr scp ncftp

ZFC_WELCOME_STRING="ZSH Foundation Configuration."
## Functions
zfc_welcome_msg () {
	echo "\r\e[0;37m${ZFC_WELCOME_STRING}\e[0m - `hostname`\e[0K"
}
## Get Going
if [ $SHLVL -eq 1 ]; then
	zfc_welcome_msg
fi


[[ $EMACS = t ]] && unsetopt zle # For zsh to work well within Emacs

bindkey '^L' push-line

source ~/.aliases
source ~/.exports

autoload -U compinit zrecompile zmv

zsh_cache=${HOME}/.zsh_cache
mkdir -p $zsh_cache

if [ $UID -eq 0 ]; then
        compinit
else
        compinit -d $zsh_cache/zcomp-$HOST

        for f in ~/.zshrc $zsh_cache/zcomp-$HOST; do
                zrecompile -p $f && rm -f $f.zwc.old
        done
fi

# Load config directiory .zsh.d
setopt extended_glob
for zshrc_snipplet in ~/.zsh.d/S[0-9][0-9]*[^~] ; do
        source $zshrc_snipplet
done
