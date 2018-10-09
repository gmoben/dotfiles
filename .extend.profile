#-*- mode: shell-script -*-
source $HOME/.aliases

if [[ -f $HOME/.xmodmap ]]; then
    xmodmap $HOME/.xmodmap
fi

if [[ -f /usr/bin/virtualenvwrapper.sh ]]; then
    source /usr/bin/virtualenvwrapper.sh
elif [[ -f /usr/local/bin/virtualenvwrapper.sh ]]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi

# Paths
export GMOCODE=/code
export GOPATH=$GMOCODE/go
export PATH=$HOME/.cargo/bin:$GOPATH/bin:$HOME/.cabal/bin:$HOME/.yarn/bin:$HOME/.local/bin:$PATH

# Virtualenv
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$GMOCODE

#bindkey '^[[1;5D' backward-word
#bindkey '^[[1;5C' forward-word

# Vagrant
export VAGRANT_LOG=warn

# SSH
export SSH_KEY_PATH=$HOME/.ssh/rsa_id

# GPG
export GPG_TTY=$(tty)
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# Editors
get_path() {
    echo $(env which --skip-alias $1)
    return $?
}

if [[ `get_path emacs` ]]; then
    export EDITOR="$(get_path emacs) -nw"
elif [[ `get_path vim` ]]; then
    export EDITOR=$(get_path vim)
else
    export EDITOR=$(get_path vim)
fi
export SYSTEMD_EDITOR=$EDITOR
