[ -f $HOME/.Xresources ] && xrdb -merge -I$HOME ~/.Xresources
[ -f $HOME/.Xdefaults ] && xrdb -merge -I$HOME ~/.Xdefaults
[ -f /etc/xprofile ] && . /etc/xprofile
[ -f $HOME/.xprofile ] && . $HOME/.xprofile
[ -f $HOME/.xmodmap ] && xmodmap $HOME/.xmodmap

exec i3
