#!/bin/bash
declare -A params
params[eDP]="--primary --mode 2880x1800 --pos 0x0 --rotate normal"

args=""
for key in "${!params[@]}"; do
    args="$args --output $key ${params[$key]}"
done

echo $args
