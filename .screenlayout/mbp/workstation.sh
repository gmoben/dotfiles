#!/bin/bash

eDP="--mode 2880x1800 --pos 0x1295 --rotate normal"
DP="--primary --mode 3840x1600 --pos 2880x0 --rotate normal --scale 1.5"
xrandr \
    --output eDP $eDP \
    --output DisplayPort-0 $DP \
    --output DisplayPort-1 --off \
    --output DisplayPort-2 --off \
    --output DisplayPort-3 --off || \
    xrandr \
        --output eDP $eDP \
        --output DisplayPort-0 --off \
        --output DisplayPort-1 $DP \
        --output DisplayPort-2 --off \
        --output DisplayPort-3 --off || \
    xrandr \
        --output eDP $eDP \
        --output DisplayPort-0 --off \
        --output DisplayPort-1 --off \
        --output DisplayPort-2 $DP \
        --output DisplayPort-3 --off || \
    xrandr \
        --output eDP $eDP \
        --output DisplayPort-0 --off \
        --output DisplayPort-1 --off \
        --output DisplayPort-2 --off \
        --output DisplayPort-3 $DP

sleep 1

[[ -f $HOME/.cache/wal/set_nitrogen.sh ]] && $HOME/.cache/wal/set_nitrogen.sh
