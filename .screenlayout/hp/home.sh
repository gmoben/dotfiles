#!/usr/bin/env bash

eDP="--mode 1920x1080 --pos 0x1080 --rotate normal"
DP="--primary --mode 3840x1600 --rate 30 --pos 1920x0 --rotate normal"
xrandr \
    --output eDP1 $eDP \
    --output DP1 $DP \
    --output HDMI1 --off \
    --output HDMI2 --off \
    --output HDMI-1 --off \
    --output HDMI-2 --off

sleep 1

[[ -f $HOME/.cache/wal/set_nitrogen.sh ]] && $HOME/.cache/wal/set_nitrogen.sh
