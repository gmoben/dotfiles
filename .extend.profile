#!/usr/bin/env zsh

# PATHS
export CODEBEN=/code/ben
export GOPATH=$GMOCODE/go
export PATH=$HOME/.cargo/bin:$GOPATH/bin:$HOME/.cabal/bin:${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/bin:$HOME/.yarn/bin:$HOME/.local/bin:$PATH

source $CODEBEN/dotfiles/.shutils
source $HOME/.aliases

# Virtualenv
export VIRTUALENVWRAPPER_PYTHON=`which python3`
export WORKON_HOME=$HOME/.virtualenvs

#bindkey '^[[1;5D' backward-word
#bindkey '^[[1;5C' forward-word

# Vagrant
export VAGRANT_LOG=warn

# SSH
export SSH_KEY_PATH=$HOME/.ssh/rsa_id

# GPG
export GPG_TTY=$(tty)

if [[ "$OSTYPE" == "linux-gnu" && -n "$XDG_RUNTIME_DIR" && -e $XDG_RUNTIME_DIR/ssh-agent.socket ]]; then
    export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
fi

# OSX PATH specifics
if [[ "$OSTYPE" == "darwin"* ]]; then
    # nodejs dependency icu4u is keg-only
    export PATH="/usr/local/opt/icu4c/bin:/usr/local/opt/icu4c/sbin:$PATH"
fi

# Editors
get_path() {
    echo $(/usr/bin/env which $1)
    return $?
}

if [[ -z "$EDITOR" ]]; then
    if [[ `get_path emacs` ]]; then
        export EDITOR="$(get_path emacsclient) -t"
    elif [[ `get_path vim` ]]; then
        export EDITOR=$(get_path vim)
    else
        export EDITOR=$(get_path nano)
    fi
fi

export SYSTEMD_EDITOR=$EDITOR

[[ -e `command -v thefuck` ]] && eval $(thefuck --alias)

if [[ -f /usr/bin/virtualenvwrapper.sh ]]; then
    source /usr/bin/virtualenvwrapper.sh
elif [[ -f /usr/local/bin/virtualenvwrapper.sh ]]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi

if [[ -e `command -v wal` ]]; then
    (cat ~/.cache/wal/sequences &)
    source ~/.cache/wal/colors-tty.sh
fi

export PATH=$PATH:$CODEBEN/dotfiles/.scripts

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

export PATH=`echo $PATH | dedup :`
