# -*- mode: sh -*-
# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=50000
SAVEHIST=50000
setopt INC_APPEND_HISTORY
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall

if [ TERM="xterm" ]; then
  export TERM='xterm-color'
fi

# what progs can use the hostname completion
compctl -k hostnames ping telnet ftp nslookup ssh traceroute mtr scp ncftp

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
  # TODO: why doesn't carthage autocomplete work when starting a shell, but if I manually run compinit it does?
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
if [ $(uname) = 'Darwin' ]; then
  if [ -f `brew --prefix`/etc/autojump ]; then
    source `brew --prefix`/etc/autojump
  fi
fi

# OPAM configuration
#. /Users/james/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

### Added by the Heroku Toolbelt
#export PATH="/usr/local/heroku/bin:$PATH"

#[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export SWIVM_DIR="/home/james/.swivm"
[ -s "$SWIVM_DIR/swivm.sh" ] && . "$SWIVM_DIR/swivm.sh"  # This loads swivm
