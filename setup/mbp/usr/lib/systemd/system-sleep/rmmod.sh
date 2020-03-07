#!/bin/sh
if [ "${1}" == "pre" ]; then
        rmmod thunderbolt
        rmmod apple_ib_tb
elif [ "${1}" == "post" ]; then
        modprobe apple_ib_tb
        modprobe thunderbolt
	if [ -f /code/ben/dotfiles/.xmodmap ]; then
	    xmodmap /code/ben/dotfiles/.xmodmap
	elif [ -f /code/ben/dotfiles/.Xmodmap ]; then
	    xmodmap /code/ben/dotfiles/.xmodmap
	fi
fi
