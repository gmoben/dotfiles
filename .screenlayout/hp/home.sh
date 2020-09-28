#!/usr/bin/env bash
declare -A params
params[eDP1]="--mode 1920x1080 --pos 0x1080 --rotate normal"
params[DP1]="--primary --mode 3840x1600 --rate 30 --pos 1920x0 --rotate normal"

args=""
for key in "${!params[@]}"; do
    args="$args --output $key ${params[$key]}"
done

echo $args
