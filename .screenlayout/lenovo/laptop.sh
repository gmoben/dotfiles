#!/usr/bin/env bash
declare -A params

# params[HDMI-0]="--off"
# params[DP-1.1]="--off"
# params[DP-1.2]="--off"
params[eDP1]="--primary --auto"
params[eDP-1-1]=params[eDP1]

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
