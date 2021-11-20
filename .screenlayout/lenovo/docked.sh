#!/bin/bash
declare -A params

params[eDP-1-1]="--mode 1920x1080 --pos 0x520 --rotate normal"
#params[HDMI-0]="--primary --mode 3840x1600 --pos 1920x0 --rotate normal --rate 30"
params[DP-1.2]="--primary --mode 3840x1600 --pos 1920x0 --rotate normal --rate 60"

args=""
for key in "${!params[@]}"; do
    args="$args --output $key ${params[$key]}"
done

echo $args
