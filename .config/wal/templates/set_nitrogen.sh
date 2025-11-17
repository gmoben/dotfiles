for i in $(seq 0 `xrandr --listactivemonitors | grep Monitors: | awk '{{print $2 - 1}}'`); do
    sleep 0.5
    nitrogen --set-zoom-fill --head=$i "{wallpaper}" 2>/dev/null
done
