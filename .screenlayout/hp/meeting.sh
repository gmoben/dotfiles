#!/usr/bin/env bash

eDP="--primary --mode 1920x1080 --pos 1920x0 --rotate normal"
HDMI="--mode 1920x1080 --pos 0x0 --rotate normal"
xrandr \
    --output eDP-1 $eDP \
    --output DP-1 --off \
    --output DP-2 --off \
    --output DP-2-1 --off \
    --output DP-2-2 --off \
    --output DP-2-3 --off \
    --output HDMI-1 $HDMI \
    --output HDMI-2 --off

sleep 1

[[ -f $HOME/.cache/wal/set_nitrogen.sh ]] && $HOME/.cache/wal/set_nitrogen.sh
