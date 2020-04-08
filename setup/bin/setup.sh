#! /usr/bin/env bash

set -o errexit
set -o pipefail

source ./utils

SETUPDIR="$(echo '`cd $(dirname $0); pwd -P`/../..')"
PLATFORM=$(eval python -mplatform | grep -iE 'arch|Ubuntu|MANJARO')
case $PLATFORM in
    *arch*|*MANJARO*)
        DISTRO=arch
        INSTALL_CMD='pacaur -S --noconfirm';;
    *Ubuntu*)
        DISTRO=ubuntu
        INSTALL_CMD='apt install -y';;
    *Darwin*)
        DISTRO=darwin
        INSTALL_CMD='brew install';;
    *)
        DISTRO=unsupported
        error "Unsupported platform $(bold $PLATFORM) detected";;
esac


warning "Platform Detected" $PLATFORM
warning "Distribution Detected" $DISTRO

yesno "Is this a Macbook (15,1)?" && IS_MBP=0 || IS_MBP=1

function install_pkg {
    local usage="Usage: install_pkg PKGNAME"
    1=${1?usage}
    $INSTALL_CMD $1
}

function bootstrap {
    sudo mkdir -m777 -p /code

    mkdir -p /code/ben && \
        mkdir -p /code/ext

    case $DISTRO in
        arch)
            info "Installing pacaur..."
            sudo pacman -S --noconfirm pacaur

            info "Downloading antigen.zsh"
            sudo mkdir -p /usr/share/zsh-antigen
            sudo curl -o /usr/share/zsh-antigen/antigen.zsh -sL git.io/antigen
            ;;
        ubuntu)
            info "Installing pacapt + symlinking to /usr/local/bin/pacman..."
            sudo wget -O /usr/local/bin/pacapt https://github.com/icy/pacapt/raw/ng/pacapt && \
                sudo chmod 755 /usr/local/bin/pacapt
            sudo ln -sv /usr/local/bin/pacapt /usr/local/bin/pacman || true
            sudo echo "sudo /usr/local/bin/pacapt" > /usr/local/bin/pacaur && \
                sudo chmod 755 /usr/local/bin/pacaur

            info "Adding PPAs"
            for src in $(cat $SETUPDIR/ubuntu/apt.ppa); do
                sudo add-apt-repository $src
            done

            info "Updating package sources"
            sudo apt-get update

            info "Installing kitty from source"
            curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin
            ln -s $HOME/.local/kitty.app/bin/kitty $HOME/.local/bin/
           ;;
        *) ;;
    esac
}

function install_base {
    info "Installing base packages"

    case $DISTRO in
        arch)
            PKGLIST=`cat $SETUPDIR/arch/pacaur.pkglist | xargs`
            ;;
        ubuntu)
            PKGLIST=`cat $SETUPDIR/ubuntu/apt.pkglist | xargs`
            warning "Warning: No MBP $DISTRO support yet."
            ;;
        *)
            error "Sorry, $DISTRO isn't supported."
            exit 1
            ;;
    esac

    $INSTALL_CMD $PKGLIST

    diff-so-fancy --set-defaults
}

function install_extras {
    info 'Installing extras'

    sudo cp -f {$SETUPDIR/mbp,}/etc/pacman.conf && \
        chown root:root /etc/pacman.conf

    $INSTALL_CMD $PKGLIST_EXTRAS

    mods=("apple-bce" "apple-ib-tb" "apple-ib-als" "applesmc")
    for mod in "${mods[@]}"; do
        sudo modprobe $mod
        sudo echo $mod >> /etc/modules-load.d/modules.conf
    done

    # Setup pulseaudio
    # https://gist.github.com/MCMrARM/c357291e4e5c18894bea10665dcebffb
    sudo cp ${$SETUPDIR/arch/mbp/root,}/usr/share/alsa/cards/AppleT2.conf
    sudo cp ${$SETUPDIR/arch/mbp/root,}/usr/share/pulseaudio/alsa-mixer/profile-sets/apple-t2.conf
    sudo cp ${$SETUPDIR/arch/mbp/root,}/usr/lib/udev/rules.d/91-pulseaudio-custom.rules

    yesno "Do you have a Macbook15,1?" && (
        sudo cp ${$SETUPDIR/arch/mbp/root,}/lib/firmware/bcrm/* /lib/firmware/bcrm
        sudo modprobe -r bcrmfmac; sudo modprobe bcrmfmac
        sudo echo bcrmfmac >> /etc/modules-load.d/apple.conf
        sudo cat << EOF >> /etc/NetworkManager/NetworkManager.conf
[device]
wifi.backend=iwd
EOF
        sudo systemctl enable --now iwd && \
            systemctl restart NetworkManager && \
            systemctl enable --now NetworkManager
    ) || warning "Skipping wifi driver installation" ;;
    esac
}

function pip_packages {
    sudo pip install -r $SETUPDIR/requirements.txt
}

function install_dotfiles {
    python $SETUPDIR/install_dotfiles.py
}

function activate_systemd {
    services=`ls $HOME/.config/systemd/user | grep -v wants | cut -d'.' -f1 | xargs`
    services="$services pulseaudio keybase keybase-redirector keybase-gui kbfs"
    info "Enabling and starting systemd user services: $services"
    systemctl --user --now enable $services
}

function install_packages {
    install_base

    [[ "$DISTRO" == 'arch' && "$IS_MBP" == "0" ]] && install_extras || warning "No extras to install for distribution $DISTRO";;

    pip_packages
    install_dotfiles
    compile_terminfo
}

function compile_terminfo {
    # Compiles xterm-24bit terminfo for emacs -nw
    sudo tic -x -o /usr/share/terminfo $SETUPDIR/terminfo/xterm-24bit.terminfo
}

function main {
    bootstrap
    install_packages
    activate_systemd
    xdg-settings set default-web-browser firefox.app
}

main
