#!/usr/bin/env bash
declare -A params
params[eDP-1]="--primary --mode 1920x1200 --pos 1626x1440 --rotate normal"
params[eDP1]=${params[eDP-1]}
params[DVI-I-1-1]="--mode 2560x1440 --pos 0x0 --rotate normal"
params[DVI-I-2-2]="--mode 2560x1440 --pos 2560x0 --rotate normal"
params[DVI-I-3-3]=${params[DVI-I-1-1]}
params[DVI-I-4-4]=${params[DVI-I-2-2]}

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
