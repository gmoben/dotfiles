# Usage: ./install.sh [project_path ($HOME/code/ben/dotfiles/)] [destination ($HOME)]

DOTFILES=$HOME/code/ben/dotfiles
DEST=$HOME

if [[$# -ge 1]]; then
    DOTFILES=$1
fi
if [[$# -ge 2]]; then
    DEST=$2
fi

echo "This script will attempt to create symlinks from files/folders in $DOTFILES to $DEST"
echo "Are you sure?"
read yn
case $yn in
    y|Y) ;;
    *) echo "Aborting"
	exit 1
	;;
esac

echo "Clobber existing symlinks?"
read yn
case $yn in
    y|Y) echo "Removing existing symlinks..."
	rm $DEST/{.tmux,.tmux.conf,.aliases,.emacs.conf,.pylintrc/.zshrc}
	;;
    *) echo "Not replacing existing symlinks"
	exit 1
	;;
esac

ln -s {$DOTFILES,$DEST}/.tmux
ln -s {$DOTFILES,$DEST}/.tmux.conf
ln -s {$DOTFILES,$DEST}/.aliases
ln -s {$DOTFILES,$DEST}/.emacs.conf
ln -s {$DOTFILES,$DEST}/.pylintrc
ln -s {$DOTFILES,$DEST}/.zshrc

if [ -d $DEST/.emacs.d ]; then
    echo "The directory $DEST/.emacs.d already exists. Delete and replace? (y/n)"
    read yn
    case $yn in
	y|Y) echo "Removing $DEST/emacs.d"
	     rm -rf $DEST/.emacs.d
	     echo "Copying $DOTFILES/.emacs.d to $DEST"
	     ln -s $DOTFILES/.emacs.d $DEST/.emacs.d
	     ;;
	* ) "Skipping .emacs.d installation";;
    esac
fi

echo "Installation complete."
echo "NOTE: iTerm configuration must be manually set to $DOTFILES/com.googlecode.iterm2.plist in iTerm settings"
