[[ -f ~/.xmodmap ]] && xmodmap ~/.xmodmap
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
[[ -f ~/.Xdefaults ]] && xrdb -merge -I$HOME ~/.Xdefaults

xset mouse 2 0

exec i3
