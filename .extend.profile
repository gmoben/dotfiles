#!/bin/bash

#########
# PATHS #
#########
export CODEBEN=/code/ben
export CODEEXT=/code/ext
export PATH=$HOME/.local/bin:$CODEBEN/dotfiles/.local/bin:$PATH

# ## Utilities + Aliases ##
source $CODEBEN/dotfiles/.shutils
source $HOME/.aliases

## NodeJS ##
export PATH=$HOME/.yarn/bin:$PATH

## Golang ##
export GOPATH=$CODEBEN/go
export PATH=$GOPATH/bin:$PATH

## Rust ##
export PATH=$HOME/.cargo/bin:$PATH

## Haskell ##
export PATH=$HOME/.cabal/bin:$PATH
export PATH=${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/bin:$PATH

## Pyenv && Virtualenv ##
export VIRTUALENVWRAPPER_PYTHON=`which python3`
export WORKON_HOME=$HOME/.pyenv/versions

if [[ -f /usr/bin/virtualenvwrapper.sh ]]; then
    source /usr/bin/virtualenvwrapper.sh
elif [[ -f /usr/local/bin/virtualenvwrapper.sh ]]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi

## Pyenv ##
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"
export PYENV_ROOT=$HOME/.pyenv
export PATH=$PYENV_ROOT/bin:$PATH

eval "$(pyenv init -)" || true
eval "$(pyenv virtualenv-init -)" || true

# ############
# # SETTINGS #
# ############
export VAGRANT_LOG=warn
export GPG_TTY=$(tty)

## Editors ##
export EDITOR=$(command -v emacsclient | xargs -IXXX echo XXX -t || command -v emacs || command -v vim || command -v nano)
export SYSTEMD_EDITOR=$EDITOR

# ## Fix M-left && M-right ##
# bindkey '^[[1;5D' backward-word
# bindkey '^[[1;5C' forward-word

## SSH ##
export SSH_KEY_PATH=$HOME/.ssh/rsa_id
if [[ "$OSTYPE" == "linux-gnu" && -n "$XDG_RUNTIME_DIR" && -e $XDG_RUNTIME_DIR/ssh-agent.socket ]]; then
    export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
fi

## OSX ##
if [[ "$OSTYPE" == "darwin"* ]]; then
    # nodejs dependency icu4u is keg-only
    export PATH="/usr/local/opt/icu4c/bin:/usr/local/opt/icu4c/sbin:$PATH"
fi

## Bootstrap Utilities ###
[[ -e `command -v thefuck` ]] && eval $(thefuck --alias)

if [[ -e `command -v wal` ]]; then
    (cat ~/.cache/wal/sequences &)
    source ~/.cache/wal/colors-tty.sh
fi

## Dedup path entries ##
export PATH=$(echo $PATH | dedup ":")
