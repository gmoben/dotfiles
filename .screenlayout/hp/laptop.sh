#!/bin/bash
declare -A params
params[eDP1]="--primary --mode 1920x1080 --pos 0x1088 --rotate normal"

args=""
for key in "${!params[@]}"; do
    args="$args --output $key ${params[$key]}"
done

echo $args
