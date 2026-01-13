#!/bin/sh

# Source secrets file if it exists (not tracked in git)
[ -f ~/.secrets ] && . ~/.secrets

#########
# PATHS #
#########
export CODEBEN=/code/ben
export CODEEXT=/code/ext
export PATH=$HOME/.local/bin:$CODEBEN/dotfiles/.local/bin:$PATH

# ## Utilities + Aliases ##
. $CODEBEN/dotfiles/.shutils
. $HOME/.aliases

## Always ensure there is an active auth agent (gpg-agent preferred, ssh-agent fallback)
. persist-auth-agent

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
export VIRTUALENVWRAPPER_PYTHON=$(which python3)
export WORKON_HOME=$HOME/.pyenv/versions
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"
export PYENV_ROOT=$HOME/.pyenv
export AUTOSWITCH_VIRTUAL_ENV_DIR=$WORKON_HOME

## Claude Code ##
export claude="DISABLE_ERROR_REPORTING=1 DISABLE_TELEMETRY=1 claude"

## Sudo Askpass (GUI prompt when no TTY available) ##
## Wrapper script at .local/bin/sudo handles TTY detection automatically
if ! command -v rofi >/dev/null 2>&1 && ! command -v zenity >/dev/null 2>&1; then
    echo "Warning: Neither rofi nor zenity found. GUI sudo prompts will not work." >&2
fi

# ############
# # SETTINGS #
# ############
export VAGRANT_LOG=warn
export GPG_TTY=$(tty)
# Signal to pinentry that we prefer terminal-based input when in a real TTY
# This is checked by ~/.config/pinentry/preexec
export PINENTRY_USER_DATA="USE_TTY=1"
# Notify gpg-agent of the current TTY for pinentry prompts
gpg-connect-agent updatestartuptty /bye >/dev/null 2>&1
## Editors ##
export EDITOR=$(command -v emacsclient > /dev/null && printf '%s\n' 'emacsclient -t' || command -v emacs > /dev/null && printf '%s\n' 'emacs' || command -v vim > /dev/null && printf '%s\n' 'vim' || command -v nano > /dev/null && printf '%s\n' 'nano')
export SYSTEMD_EDITOR=$EDITOR

## OSX ##
case "$OSTYPE" in
darwin*)
    # nodejs dependency icu4u is keg-only
    export PATH="/usr/local/opt/icu4c/bin:/usr/local/opt/icu4c/sbin:$PATH"
    ;;
esac

## Bootstrap Utilities ###
if [ -x "$(command -v thefuck)" ]; then
    eval $(thefuck --alias)
fi

if [ -x "$(command -v wal)" ]; then
    [ -f ~/.cache/wal/sequences ] && (cat ~/.cache/wal/sequences &)
    [ -f ~/.cache/wal/colors-tty.sh ] && . ~/.cache/wal/colors-tty.sh
fi

if [ -x "$(command -v aws_completer)" ]; then
    complete -C '$(command -v aws_completer)' aws
fi

if [ "$(uname --nodename)" = "thinkpad-arch" ]; then
    export LIBVA_DRIVER_NAME=i965
fi

has_systemd_unit() {
    case "$(hostname)" in
        "dev"*".amazon.com") ;;
        *)
            if [ -x "$(command -v systemctl)" ]; then
                systemctl --user list-unit-files --all 2>/dev/null | grep "$@"
            fi
            ;;
    esac
}

if [ -z "$(has_systemd_unit emacs)" ] && [ -z "$(ps -ef | grep emacs | grep daemon)" ]; then
    echo "Launching emacsd in the background..."
    emacsd &>/dev/null &
fi

## Dedup path entries ##
export PATH=$(echo "$PATH" | dedup ":")
