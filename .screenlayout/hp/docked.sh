#!/usr/bin/env bash
declare -A params
params[eDP-1]="--mode 1920x1080 --pos 3840x1376 --rotate normal"
params[DP-2-1]="--primary --mode 3840x1600 --rate 30 --pos 0x0 --rotate normal"
params[DP-2-2]=${params[DP-2-1]}

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
