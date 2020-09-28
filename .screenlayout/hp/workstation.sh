#!/bin/bash
declare -A params
params[eDP-1]="--mode 1920x1080 --pos 0x1080 --rotate normal"
params[DP-1-1]="--primary --mode 3440x1440 --pos 1920x0 --rotate normal"

args=""
for key in "${!params[@]}"; do
    args="$args --output $key ${params[$key]}"
done

echo $args
