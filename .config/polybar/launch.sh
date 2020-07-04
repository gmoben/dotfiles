#!/usr/bin/env bash

PRIMARY_MONITOR=$(polybar -m|grep "(primary)"|sed -e 's/:.*$//g')
SECONDARY_MONITOR=$(polybar -m|grep -v "(primary)"|sed -e 's/:.*$//g')

# Terminate already running bar instances
killall -q polybar
# If all your bars have ipc enabled, you can also use
# polybar-msg cmd quit

launch_bar() {
    local logfile="/tmp/polybar-$1.log"
    echo "---" | tee -a $logfile
    polybar $1 >>$logfile 2>&1 &
    echo "Launched polybar $1"
}

launch_bar primary
launch_bar secondary
