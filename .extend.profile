#-*- mode: shell-script -*-
source $HOME/.aliases

if [[ -f $HOME/.xmodmap ]]; then
   (xmodmap $HOME/.xmodmap &>/dev/null 2>&1)
fi

if [[ -f /usr/bin/virtualenvwrapper.sh ]]; then
    source /usr/bin/virtualenvwrapper.sh
elif [[ -f /usr/local/bin/virtualenvwrapper.sh ]]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi


# Shortcuts
export CODEBEN=/code/ben/
# Paths
export GMOCODE=/code
export GOPATH=$GMOCODE/go
export PATH=$HOME/.cargo/bin:$GOPATH/bin:$HOME/.cabal/bin:${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/bin:$HOME/.yarn/bin:$HOME/.local/bin:$PATH

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

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
fi

# OSX PATH specifics
if [[ "$OSTYPE" == "darwin"* ]]; then
    # nodejs dependency icu4u is keg-only
    export PATH="/usr/local/opt/icu4c/bin:/usr/local/opt/icu4c/sbin:$PATH"
fi

# Editors
get_path() {
    echo $(env which $1)
    return $?
}

if [[ `get_path emacs` ]]; then
    export EDITOR="$(get_path emacs) -nw"
elif [[ `get_path vim` ]]; then
    export EDITOR=$(get_path vim)
else
    export EDITOR=$(get_path nano)
fi
export SYSTEMD_EDITOR=$EDITOR
