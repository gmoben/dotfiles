source $HOME/.aliases
source /usr/bin/virtualenvwrapper.sh

export EDITOR=emacsc
export SYSTEMD_EDITOR=emacsc
export SSH_KEY_PATH=$HOME/.ssh/rsa_id
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/code

export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$HOME/.cabal/bin:$HOME/.yarn/bin:$HOME/.local/bin:$PATH

bindkey '^[[1;5D' backward-word
bindkey '^[[1;5C' forward-word

export VAGRANT_LOG=warn
