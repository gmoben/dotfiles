[ -f $HOME/.Xresources ] && xrdb -merge -I$HOME ~/.Xresources
[ -f $HOME/.Xdefaults ] && xrdb -merge -I$HOME ~/.Xdefaults
[ -f /etc/xprofile ] && . /etc/xprofile
[ -f $HOME/.xprofile ] && . $HOME/.xprofile

exec i3
