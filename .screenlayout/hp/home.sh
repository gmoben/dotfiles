#!/usr/bin/env bash
declare -A params
params[eDP1]="--mode 1920x1080 --pos 0x1080 --rotate normal"
params[DP1]="--primary --mode 3840x1600 --rate 30 --pos 1920x0 --rotate normal"

connected=$(xrandr -q | grep " connected " | awk '{print $1}')
disconnected=$(xrandr -q | grep " disconnected " | awk '{print $1}')

args=""
for output in $connected; do
    p=`[[ ${params[$output]} ]] && echo ${params[$output]} || echo '--off'`
    args="$args --output $output $p"
done

for output in $disconnected; do
    args="$args --output $output --off"
done

xrandr $args
