#!/usr/bin/env bash

eDP="--mode 1920x1080 --pos 0x1080 --rotate normal"
DP="--primary --mode 3440x1440 --pos 1920x0 --rotate normal"
xrandr \
    --output eDP-1 $eDP \
    --output DP-1 --off \
    --output DP-1-1 $DP \
    --output DP-1-2 --off \
    --output DP-1-3 --off \
    --output DP-2 --off \
    --output HDMI-1 --off \
    --output HDMI-2 --off ||
    xrandr \
        --output eDP-1 $eDP \
        --output DP-1 --off \
        --output DP-1-1 --off \
        --output DP-1-2 $DP \
        --output DP-1-3 --off \
        --output DP-2 --off \
        --output HDMI-1 --off \
        --output HDMI-2 --off

sleep 1

[[ -f $HOME/.cache/wal/set_nitrogen.sh ]] && $HOME/.cache/wal/set_nitrogen.sh
