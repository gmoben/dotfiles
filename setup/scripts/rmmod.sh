#!/bin/sh
if [ "${1}" == "pre" ]; then
        rmmod thunderbolt
        rmmod apple_ib_tb
elif [ "${1}" == "post" ]; then
        modprobe apple_ib_tb
        modprobe thunderbolt
fi
