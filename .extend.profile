#-*- mode: shell-script -*-
source $HOME/.aliases

if [[ -f /usr/bin/virtualenvwrapper.sh ]] ; then
    source /usr/bin/virtualenvwrapper.sh
fi

export CODE=/code

export EDITOR=emacs
export SYSTEMD_EDITOR=emacs
export SSH_KEY_PATH=$HOME/.ssh/rsa_id
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/code

export GOPATH=/code/go
export PATH=$HOME/.cargo/bin:$GOPATH/bin:$HOME/.cabal/bin:$HOME/.yarn/bin:$HOME/.local/bin:$PATH

#bindkey '^[[1;5D' backward-word
#bindkey '^[[1;5C' forward-word

export VAGRANT_LOG=warn

export GPG_TTY=$(tty)

export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
