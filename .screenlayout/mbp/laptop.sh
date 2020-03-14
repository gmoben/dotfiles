#!/bin/bash
xrandr --output eDP --primary --mode 2880x1800 --pos 0x0 --rotate normal --output DisplayPort-0 --off --output DisplayPort-1 --off --output DisplayPort-2 --off --output DisplayPort-3 --off

sleep 1

[[ -f $HOME/.cache/wal/set_nitrogen.sh ]] && $HOME/.cache/wal/set_nitrogen.sh
