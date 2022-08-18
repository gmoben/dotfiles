#!/usr/bin/env bash
declare -A params
params[eDP]="--mode 2880x1800 --pos 0x1295 --rotate normal"
params[DisplayPort-1]="--primary --mode 3840x1600 --pos 2880x0 --rotate normal --scale 1.5"

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
