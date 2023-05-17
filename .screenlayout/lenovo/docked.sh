#!/usr/bin/env bash
declare -A params

params[eDP-1-1]="--mode 1920x1080 --pos 0x520 --rotate normal"
#params[HDMI-0]="--primary --mode 3840x1600 --pos 1920x0 --rotate normal --rate 30"
params[DP-1.2]="--mode 3840x1600 --pos 1920x0 --rotate normal --rate 60"

connected=$(xrandr -q | grep " connected " | awk '{print $1}')
disconnected=$(xrandr -q | grep " disconnected " | awk '{print $1}')

if [[ $connected == "eDP-1-1" ]]; then
    params[eDP-1-1]="--primary ${params[eDP-1-1]}"
elif [[ $connected =~ "DP-1.2" ]]; then
    params[DP-1.2]="--primary ${params[DP-1.2]}"
fi

args=""
for output in $connected; do
    p=`[[ ${params[$output]} ]] && echo ${params[$output]} || echo '--off'`
    args="$args --output $output $p"
done

for output in $disconnected; do
    args="$args --output $output --off"
done

echo $args
