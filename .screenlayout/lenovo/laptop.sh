#!/bin/bash
declare -A params
params[HDMI-0]="--off"
params[eDP-1-1]="--auto"

args=""
for key in "${!params[@]}"; do
    args="$args --output $key ${params[$key]}"
done

echo $args
