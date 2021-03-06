#!/usr/bin/bash

#######################
# String Manipulation #
#######################
split() {
    if [[ $1 == "" ]]; then
        1="()"
    else
        1="('$1')"
    fi

    python3 -c "import sys; sys.stdout.write('\n'.join([x for line in sys.stdin.readlines() for x in line.split$1]))"
}

_repeat() {
    usage="_repeat [str] [count]"
    str=${1?$usage}
    count=${2?$usage}
    echo $(python3 -c "print('${str}' * ${count})")
}

###################
# Text Formatting #
###################
red() {
    printf "\033[31m$@\033[39m"
}

green() {
    printf "\033[32m$@\033[39m"
}

yellow() {
    printf "\033[33m$@\033[39m"
}

blue() {
    printf "\033[34m$@\033[39m"
}

bold() {
    printf "\033[1m$@\033[0m"
}

###########
# Logging #
###########
log() {
    if [[ $# -ge 3 ]]; then
        [[ $1 =~ "-.*" ]] && opts="$1"
        shift
    else
        opts=''
    fi

    if [[ $# -ge 2 ]]; then
	headline="$1" && shift
	while [[ $# -ge 2 ]]; do
	    headline="$headline $1" && shift
	done
    else 
	headline='LOG';
    fi
    msg=${1?"Usage: log [-n] [HEADLINE] MESSAGE"}
    echo $opts `bold "$headline"` $msg
}

info() {
    [[ $# -ge 3 ]] && opts=$1 && shift ||  opts=''
    [[ $# -ge 2 ]] && headline=$1 && shift || headline='INFO'
    [[ ! $headline =~ "[.+]" ]] && headline="[$headline]"
    local msg=${1?"Usage: $0 [OPTS] [HEADLINE] MESSAGE"}
    log "$opts" `blue "$headline"` $msg
}

success() {
    [[ $# -ge 3 ]] && opts="$1" && shift ||  opts=''
    [[ $# -ge 2 ]] && headline="$1" && shift || headline='SUCCESS'
    [[ ! $headline =~ "[.+]" ]] && headline=[$headline]
    msg=${1?"Usage: $0 [OPTS] [HEADLINE] MESSAGE"}
    log "$opts" `green "$headline"` $msg
}

warning() {
    [[ $# -ge 3 ]] && opts=$1 && shift ||  opts=''
    [[ $# -ge 2 ]] && headline=$1 && shift || headline='WARNING'
    [[ ! $headline =~ "[.+]" ]] && headline=[$headline]
    msg=${1?"Usage: $0 [OPTS] [HEADLINE] MESSAGE"}
    log "$opts" `yellow "$headline"` $msg
}

error() {
    [[ $# -ge 3 ]] && opts=$1 && shift ||  opts=''
    [[ $# -ge 2 ]] && headline=$1 && shift || headline='ERROR'
    [[ ! $headline =~ "[.+]" ]] && headline="[$headline]"
    msg=${1?"Usage: $0 [OPTS] [HEADLINE] MESSAGE"}
    log "$opts" `red "$headline"` $msg
}

execute() {
    [[ $# -ge 3 ]] && headline=$1 && shift || headline='EXECUTING'
    [[ $# -ge 2 ]] && msg=$1 && shift || msg="$1"
    cmd=${@?"Usage: execute [HEADLINE] [MESSAGE] CMD"}
    warning -n $headline "$msg "
    $(eval $cmd &>/dev/null) && echo `bold $(green 'SUCCESS')` || echo `bold $(red 'FAILURE')`
}

###########
# Helpers #
###########
do_expect() {
    printf '%s' "$1" | /usr/bin/expect -
}

yesno() {
    local usage="Usage: yesno QUESTION [ON_YES] [ON_NO] [ON_INVALID]"
    local question=${1?$usage}
    local on_yes=${2-"return 0"}
    local on_no=${3-"return 1"}
    local on_invalid=${4-'echo "Invalid answer. Try again."'}
    while true; do
	read -n 1 -p "$question (yes/no) " yn && echo
	case $yn in
	    y|Y) eval "$on_yes";;
	    n|N) eval "$on_no";;
	    *) eval "$on_invalid";;
	esac
    done
}
