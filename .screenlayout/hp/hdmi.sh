#!/bin/sh
declare -A params
params[HDMI-1]="--primary --mode 3840x1600 --pos 1920x0 --rotate normal"
params[eDP-1]="--mode 1920x1080 --pos 0x1088 --rotate normal"

args=""
for key in "${!params[@]}"; do
    args="$args --output $key ${params[$key]}"
done

echo $args
