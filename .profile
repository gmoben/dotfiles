#
# ~/.profile
#
#

[[ "$XDG_CURRENT_DESKTOP" == "KDE" ]] || export QT_QPA_PLATFORMTHEME="qt5ct"

[[ -f /usr/bin/thefuck ]] && eval $(thefuck --alias)

[[ -f $HOME/.extend.profile ]] && . $HOME/.extend.profile
