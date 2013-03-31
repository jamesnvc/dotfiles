# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall

if [ TERM="xterm" ]; then
  export TERM='xterm-color'
fi

# what progs can use the hostname completion
compctl -k hostnames ping telnet ftp nslookup ssh traceroute mtr scp ncftp

compctl -g '~/.teamocil/*(:t:r)' teamocil

[[ $EMACS = t ]] && unsetopt zle # For zsh to work well within Emacs

bindkey '^L' push-line

source ~/.aliases
source ~/.exports
# PATH fixes for rvm
if [ `which ruby` = '/usr/bin/ruby' ]; then
  export PATH=~/.rvm/bin:$PATH
fi

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
source ~/.zsh.d/syntax-highlighting-filetypes/zsh-syntax-highlighting-filetypes.zsh
# autojump
if [ -f `brew --prefix`/etc/autojump ]; then
  source `brew --prefix`/etc/autojump
fi
