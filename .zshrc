#!/bin/zsh

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Hack around oh-my-zsh since we're not using it directly,
# but we're using some plugins in antibody
ZSH_CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/ohmyzsh"
if [[ ! -d $ZSH_CACHE_DIR ]]; then
    mkdir -p $ZSH_CACHE_DIR/completions
fi

if [[ ! -d $HOME/.zfunc ]]; then
    mkdir -p $HOME/.zfunc
fi

fpath+=~/.zfunc
ZSH_PYENV_LAZY_VIRTUALENV=true

source ${ZDOTDIR:-$HOME}/.antidote/antidote.zsh
antidote load

# Load autoswitch-virtualenv only when NOT in VS Code
if [[ "$TERM_PROGRAM" != "vscode" ]]; then
    antidote bundle gmoben/zsh-autoswitch-virtualenv >/dev/null
fi

export COLORTERM=truecolor

autoload -Uz compinit colors zcalc
autoload bashcompinit
bashcompinit
compinit -d
colors

source $HOME/.profile

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

bindkey "^[[3~" delete-char                     # Key Del
bindkey "^[[H" beginning-of-line                # Key Home
bindkey "^[[F" end-of-line                      # Key End
bindkey "^[[1;3C" forward-word                  # Key Alt + Right
bindkey "^[[1;3D" backward-word                 # Key Alt + Left

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
fi

[[ `command -v fzf` ]] && source <(fzf --zsh)

export FZF_DEFAULT_OPTS="--preview 'bat --style=numbers --color=always --line-range :500 {}'"
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview'"

if [[ ! `command -v diff-so-fancy` && -d /code/ext/diff-so-fancy ]]; then
    export PATH=/code/ext/diff-so-fancy:$PATH
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Taken from output of `~/.local/bin/mise activate zsh`
if [[ `command -v mise` ]]; then
    export MISE_SHELL=zsh
    export __MISE_ORIG_PATH="$PATH"

    mise() {
      local command
      command="${1:-}"
      if [ "$#" = 0 ]; then
        command $HOME/.local/bin/mise
        return
      fi
      shift

      case "$command" in
      deactivate|s|shell)
        # if argv doesn't contains -h,--help
        if [[ ! " $@ " =~ " --help " ]] && [[ ! " $@ " =~ " -h " ]]; then
          eval "$(command $HOME/.local/bin/mise "$command" "$@")"
          return $?
        fi
        ;;
      esac
      command $HOME/.local/bin/mise "$command" "$@"
    }

    _mise_hook() {
      eval "$($HOME/.local/bin/mise hook-env -s zsh)";
    }
    typeset -ag precmd_functions;
    if [[ -z "${precmd_functions[(r)_mise_hook]+1}" ]]; then
      precmd_functions=( _mise_hook ${precmd_functions[@]} )
    fi
    typeset -ag chpwd_functions;
    if [[ -z "${chpwd_functions[(r)_mise_hook]+1}" ]]; then
      chpwd_functions=( _mise_hook ${chpwd_functions[@]} )
    fi

    if [ -z "${_mise_cmd_not_found:-}" ]; then
        _mise_cmd_not_found=1
        [ -n "$(declare -f command_not_found_handler)" ] && eval "${$(declare -f command_not_found_handler)/command_not_found_handler/_command_not_found_handler}"

        function command_not_found_handler() {
            if $HOME/.local/bin/mise hook-not-found -s zsh -- "$1"; then
              _mise_hook
              "$@"
            elif [ -n "$(declare -f _command_not_found_handler)" ]; then
                _command_not_found_handler "$@"
            else
                echo "zsh: command not found: $1" >&2
                return 127
            fi
        }
    fi

    # Generate zsh completions if they don't exist or are empty
    if [[ ! -f $HOME/.zfunc/_mise || -z `cat $HOME/.zfunc/_mise` ]]; then
        mise completions zsh > $HOME/.zfunc/_mise
        compinit -d
    fi
fi

if [[ "$TERM_PROGRAM" == "vscode" ]]; then
    vsc_server_folder=$(ls -d ~/.vscode-server/cli/servers/*/ -t | head -n1 | tail -1)
    code_cli="$vsc_server_folder/server/bin/remote-cli/code"
    if [[ -x "$code_cli" ]]; then
        VSI=$("$code_cli" --locate-shell-integration-path zsh 2>/dev/null)
        [[ -f "$VSI" ]] && . "$VSI"
    fi

    # Make `cd` from a vscode terminal go to the workspace root
    # Assume the following is in vscode settings:
    # "terminal.integrated.env.linux":  {"VSCODE_WS": "${workspaceFolder}"},
    # "terminal.integrated.env.windows":{"VSCODE_WS": "${workspaceFolder}"},
    # When in filemode / not in a workspace, `VSCODE_WS` is set to the literal `${workspaceFolder}` so we check and ignore that
    if [[ -v VSCODE_WS ]] && [[ "$VSCODE_WS" != '${workspaceFolder}' ]]; then
        function _cd() {
            # Override cd behavior: no args goes to VSCODE_WS, preserve special cases
            case "${1:-}" in
                "")      # No arguments: go to workspace root
                         cd "${VSCODE_WS}" ;;
                "-"|".") # Previous dir (-) and current dir (.) work normally
                         cd "$1" ;;
                /*)      # Absolute paths work normally
                         cd "$1" ;;
                *)       # Relative paths: prefix with workspace root
                         cd "${VSCODE_WS}" && cd "$@" ;;
            esac
        }
        alias cd=_cd
    fi

fi
