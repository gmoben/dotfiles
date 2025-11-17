#!/usr/bin/env bash
declare -A params
params[DP-3]="--primary --mode 3440x1440 --pos 0x0 --rotate normal"
params[DP3]=${params[DP-3]}
params[DP-1]=${params[DP-3]}
params[DP1]=${params[DP-3]}
params[eDP1]="--mode 1920x1200 --pos 3440x1067 --rotate normal"
params[eDP-1]=${params[eDP1]}

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
