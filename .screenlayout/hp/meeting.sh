#!/usr/bin/env bash
declare -A params
params[eDP-1]="--primary --mode 1920x1080 --pos 1920x0 --rotate normal"
params[HDMI-1]="--mode 1920x1080 --pos 0x0 --rotate normal"

args=""
for key in "${!params[@]}"; do
    args="$args --output $key ${params[$key]}"
done

echo $args
