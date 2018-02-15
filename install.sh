#!/bin/bash

# Usage: ./install.sh [project_path ($HOME/code/ben/dotfiles/)] [destination ($HOME)]

DOTFILES=$PWD
DEST=$HOME

if [ $# -ge 1 ]; then
    DOTFILES=$1
fi
if [ $# -ge 2 ]; then
    DEST=$2
fi

SYMLINKS=(
    .i3
    .Xresources
    .Xresources.d
    .aliases
    .config/systemd/user/emacs.service
    .config/systemd/user/ssh-agent.service
    .config/systemd/user/xscreensaver.service
    .emacs.conf
    .extend.Xresources
    .extend.profile
    .pam_environment
    .pylintrc
    .tmux
    .tmux.conf
    .xinitrc
    .xmodmap
    .xprofile
    .xsession
    .zshrc
)


echo "This script will attempt to create symlinks from files/folders in $DOTFILES to $DEST"
read -n 1 -p "Are you sure? (y/n)" yn
echo
case $yn in
    y|Y) ;;
    *) echo "Aborting"
	exit 1
	;;
esac

read -n 1 -p "Clobber existing symlinks? (y/n)" yn
echo
case $yn in
    y|Y)
	echo "Removing existing symlinks..."
	for link in $SYMLINKS; do
	    if [ -L $DEST/$link ]; then
	    	rm $DEST/$link
	    elif [ -f $DEST/$link ] || [ -d $DEST/$link ]; then
	        echo "$DEST/$link is not a symlink. Aborting!"
	        exit 1
	    fi
	done
	;;
    *)
	echo "Not replacing existing symlinks. Aborting!"
	exit 1
	;;
esac

echo "Creating symlinks..."
for link in $SYMLINKS; do
    ln -s {$DOTFILES,$DEST}/$link
done

read -n 1 -p "Auto-add ssh keys to ssh-agent? (y/n)" yn
case $yn in
    y|Y)
	sed -i '/s/AddKeysToAgent.+//' $DEST/.ssh/config
	echo 'AddKeysToAgent  yes' >> $DEST/.ssh/config
	;;
    *)
	echo "Not altering ssh config"
	;;
esac

echo "Downloading antigen.zsh"
curl -L git.io/antigen > $DEST/antigen.zsh

if [ -d $DEST/.emacs.d ]; then
    read -n 1 -p "The directory $DEST/.emacs.d already exists. Delete and replace? (y/n)" yn
    case $yn in
	y|Y) echo "Removing $DEST/emacs.d"
	     rm -rf $DEST/.emacs.d
	     echo "Copying $DOTFILES/.emacs.d to $DEST"
	     cp -R $DOTFILES/.emacs.d/* $DEST/.emacs.d
	     ;;
	*)
	    echo "Skipping .emacs.d installation"
	    ;;
    esac
fi

echo "Installation complete."
