#!/usr/bin/env zsh

# Hack around oh-my-zsh since we're not using it directly,
# but we're using some plugins in antibody
ZSH_CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/ohmyzsh"
if [[ ! -d $ZSH_CACHE_DIR ]]; then
    mkdir -p $ZSH_CACHE_DIR/completions
fi

fpath+=~/.zfunc
ZSH_PYENV_LAZY_VIRTUALENV=true

autoload -Uz compinit colors zcalc
autoload bashcompinit
bashcompinit
compinit -d
colors

source $HOME/.profile

if [[ ! -f $HOME/.zplugins.sh ]]; then
    source <(antibody init)
    antibody bundle < ~/.zplugins > ~/.zplugins.sh
else
    source ~/.zplugins.sh
fi

## Options section

HISTFILE=~/.zhistory
HISTSIZE=100000
SAVEHIST=50000

setopt correct           # Auto correct mistakes
setopt extendedglob      # Extended globbing. Allows using regular expressions with *
setopt nocaseglob        # Case insensitive globbing
setopt rcexpandparam     # Array expension with parameters
setopt nocheckjobs       # Don't warn about running processes when exiting
setopt numericglobsort   # Sort filenames numerically when it makes sense
setopt nobeep            # No beep
setopt autocd            # if only directory path is entered, cd there.
setopt sharehistory      # Shares history file between sessions before exit
setopt histignorealldups # If a new command is a duplicate, remove the older one
setopt histreduceblanks  # Strip commands before saving them to history

# Fuzzy-match completions >> https://superuser.com/a/815317

zstyle ':completion:*' matcher-list '' \
  'm:{a-z\-}={A-Z\_}' \
  'r:[^[:alpha:]]||[[:alpha:]]=** r:|=* m:{a-z\-}={A-Z\_}' \
  'r:|?=** m:{a-z\-}={A-Z\_}'

zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"         # Colored completion (different colors for dirs/files/etc)
zstyle ':completion:*' rehash true                              # automatically find new executables in path
# Speed up completions
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -t"
WORDCHARS=${WORDCHARS//\/[&.;]}                                 # Don't consider certain characters part of the word


## Keybindings section
bindkey -e

# bind UP and DOWN arrow keys to history substring search
zmodload zsh/terminfo &>/dev/null
if [[ "${terminfo[kcuu1]}" != "" ]]; then
    bindkey "${terminfo[kcuu1]}" history-substring-search-up
fi
if [[ "${terminfo[kcud1]}" != "" ]]; then
    bindkey "${terminfo[kcud1]}" history-substring-search-down
fi
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

bindkey '^[[7~' beginning-of-line                               # Home key
bindkey '^[[H' beginning-of-line                                # Home key
if [[ "${terminfo[khome]}" != "" ]]; then
    bindkey "${terminfo[khome]}" beginning-of-line                # [Home] - Go to beginning of line
fi
bindkey '^[[8~' end-of-line                                     # End key
bindkey '^[[F' end-of-line                                     # End key
if [[ "${terminfo[kend]}" != "" ]]; then
    bindkey "${terminfo[kend]}" end-of-line                       # [End] - Go to end of line
fi
bindkey '^[[2~' overwrite-mode                                  # Insert key
bindkey '^[[3~' delete-char                                     # Delete key
bindkey '^[[C'  forward-char                                    # Right key
bindkey '^[[D'  backward-char                                   # Left key
bindkey '^[[5~' history-beginning-search-backward               # Page up key
bindkey '^[[6~' history-beginning-search-forward                # Page down key

# Navigate words with ctrl+arrow keys
bindkey '^[Oc' forward-word                                     #
bindkey '^[Od' backward-word                                    #
bindkey '^[[1;5D' backward-word                                 #
bindkey '^[[1;5C' forward-word                                  #
bindkey '^H' backward-kill-word                                 # delete previous word with ctrl+backspace
bindkey '^[[Z' undo                                             # Shift+tab undo last action
bindkey '^[/' undo

# Theming section

# enable substitution for prompt
setopt prompt_subst

# Prompt (on left side) similar to default bash prompt, or redhat zsh prompt with colors
#PROMPT="%(!.%{$fg[red]%}[%n@%m %1~]%{$reset_color%}# .%{$fg[green]%}[%n@%m %1~]%{$reset_color%}$ "
# Maia prompt
PROMPT="%B%{$fg[cyan]%}%(4~|%-1~/.../%2~|%~)%u%b >%{$fg[cyan]%}>%B%(?.%{$fg[cyan]%}.%{$fg[red]%})>%{$reset_color%}%b " # Print some system information when the shell is first started

# Color man pages
export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'
export LESS=-r


# Use autosuggestion
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'

# https://github.com/pypa/pipx
if [[ `command -v pipx` ]]; then
    eval "$(register-python-argcomplete pipx)"
fi

[[ -f $HOME/.ssh-agent ]] && source ~/.ssh-agent &>/dev/null

# WSL2 X11 Forwarding
if [[ `uname -r` =~ microsoft-standard ]]; then
    export WINIP=$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null)
    export DISPLAY=$WINIP:0
    export LIBGL_ALWAYS_INDIRECT=1

    if [[ ! `ps -ef | grep emacs | grep daemon` ]]; then
        echo "Launching emacsd in the background..."
        emacsd &>/dev/null &
    fi
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if [[ ! `command -v diff-so-fancy` && -d /code/ext/diff-so-fancy ]]; then
    export PATH=/code/ext/diff-so-fancy:$PATH
fi
