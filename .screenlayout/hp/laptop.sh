#!/usr/bin/env bash

eDP="--primary --mode 1920x1080 --pos 0x1088 --rotate normal"
DP="--mode 3840x1600 --pos 1920x0 --rate 30 --rotate normal"
xrandr \
    --output eDP1 $eDP \
    --output eDP-1 --off \
    --output eDP-2 --off \
    --output DP1 --off \
    --output DP-1 --off \
    --output DP-2 --off \
    --output HDMI-1 --off \
    --output HDMI1 --off \
    --output HDMI-2 --off || \
    xrandr \
        --output eDP1 --off \
        --output eDP-1 $eDP \
        --output eDP-2 --off \
        --output DP1 --off \
        --output DP-1 --off \
        --output DP-2 --off \
        --output HDMI-1 --off \
        --output HDMI1 --off \
        --output HDMI-2 --off || \
    xrandr \
        --output eDP1 --off \
        --output eDP-1 --off \
        --output eDP-2 $eDP \
        --output DP1 --off \
        --output DP-1 --off \
        --output DP-2 --off \
        --output HDMI1 --off \
        --output HDMI-1 --off \
        --output HDMI-2 --off


sleep 1

[[ -f $HOME/.cache/wal/set_nitrogen.sh ]] && $HOME/.cache/wal/set_nitrogen.sh
