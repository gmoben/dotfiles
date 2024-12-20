#!/usr/bin/env bash
declare -A params
params[eDP1]="--mode 1920x1080 --pos 0x1260 --rotate normal"
params[eDP-1]=${params[eDP1]}
params[eDP-1-1]=${params[eDP1]}
params[DP1]="--primary --mode 3840x1600 --pos 1920x0 --rotate normal"
params[DP-1]=${params[DP1]}
params[DP-3]=${params[DP1]}
params[DP-1-1]=${params[DP1]}
params[DP-1-2]=${params[DP1]}
params[DP-3-3]=${params[DP1]}

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

echo $args
