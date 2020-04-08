#!/usr/bin/env bash

yesno() {
    local usage="Usage: yesno QUESTION [ON_YES] [ON_NO] [ON_INVALID]"
    local question=${1?$usage}
    local on_yes=${2-"return 0"}
    local on_no=${3-"return 1"}
    local on_invalid=${4-'echo "Invalid answer. Try again."'}
    while true; do
        read -n 1 -p "$question (yes/no)" yn
        case $yn in
            y|Y) eval "$on_yes";;
            n/N) eval "$on_no";;
            *) eval "$on_invalid";;
        esac
    done
}

split() {
    if [[ $1 == "" ]]; then
        1="()"
    else
        1="('$1')"
    fi

    python -c "import sys; sys.stdout.write('\n'.join([x for line in sys.stdin.readlines() for x in line.split$1]))"
}

###################
# Text Formatting #
###################
function red {
    printf "\033[31m$1\033[39m"
}

function green {
    printf "\033[32m$1\033[39m"
}

function yellow {
    printf "\033[33m$1\033[39m"
}

function blue {
    printf "\033[34m$1\033[39m"
}

function bold {
    printf "\033[1m$1\033[0m"
}

function log {
    if [[ $# -ge 3 ]]; then
        [[ $1 =~ "-.*" ]] && opts=$1
        shift
    else
        opts=''
    fi

    if [[ $# -ge 2 ]]; then headline=$1 && shift; else headline='LOG'; fi
    msg=${1?"Usage: log [-n] [HEADLINE] MESSAGE"}
    echo $opts `bold $headline` $msg
}

function info {
    [[ $# -ge 3 ]] && opts=$1 && shift ||  opts=''
    [[ $# -ge 2 ]] && headline=$1 && shift || headline='INFO'
    [[ ! $headline =~ "[.+]" ]] && headline="[$headline]"
    local msg=${1?"Usage: $0 [OPTS] [HEADLINE] MESSAGE"}
    log $opts `blue "$headline"` $msg
}

function success {
    [[ $# -ge 3 ]] && opts=$1 && shift ||  opts=''
    [[ $# -ge 2 ]] && headline=$1 && shift || headline='SUCCESS'
    [[ ! $headline =~ "[.+]" ]] && headline="[$headline]"
    msg=${1?"Usage: $0 [OPTS] [HEADLINE] MESSAGE"}
    log $opts `green "$headline"` $msg
}

function warning {
    [[ $# -ge 3 ]] && opts=$1 && shift ||  opts=''
    [[ $# -ge 2 ]] && headline=$1 && shift || headline='WARNING'
    [[ ! $headline =~ "[.+]" ]] && headline="[$headline]"
    msg=${1?"Usage: $0 [OPTS] [HEADLINE] MESSAGE"}
    log $opts `yellow "$headline"` $msg
}

function error {
    [[ $# -ge 3 ]] && opts=$1 && shift ||  opts=''
    [[ $# -ge 2 ]] && headline=$1 && shift || headline='ERROR'
    [[ ! $headline =~ "[.+]" ]] && headline="[$headline]"
    msg=${1?"Usage: $0 [OPTS] [HEADLINE] MESSAGE"}
    log $opts `red "$headline"` $msg
}

function execute {
    [[ $# -ge 3 ]] && headline=$1 && shift || headline='EXECUTING'
    [[ $# -ge 2 ]] && msg=$1 && shift || msg="$1"
    cmd=${@?"Usage: execute [HEADLINE] [MESSAGE] CMD"}
    warning -n $headline "$msg "
    $(eval $cmd &>/dev/null) && echo `bold $(green 'SUCCESS')` || echo `bold $(red 'FAILURE')`
}
