#!/bin/bash
declare -A params
params[eDP]="--mode 2880x1800 --pos 0x1295 --rotate normal"
params[DisplayPort-1]="--primary --mode 3840x1600 --pos 2880x0 --rotate normal --scale 1.5"

args=""
for key in "${!params[@]}"; do
    args="$args --output $key ${params[$key]}"
done

echo $args
