#! /usr/bin/env bash

set -o errexit
set -o pipefail

SETUPDIR="$(echo `cd $(dirname $0); pwd -P`)"
PLATFORM=$(eval python -mplatform | grep -iE 'arch|Ubuntu|MANJARO')
case $PLATFORM in
    *arch*|*MANJARO*) DISTRO=Arch ;;
    *Ubuntu*) DISTRO=Ubuntu ;;
    *Darwin*) DISTRO=Darwin ;;
    *) DISTRO=Unsupported ;;
esac

echo "Detected platform: $PLATFORM"
echo "Detected distribution: $DISTRO"

case $DISTRO in
    Arch)
        BASEPKGS=`cat $SETUPDIR/arch.pkglist | xargs`
        MBPPKGS=`cat $SETUPDIR/arch-mbp.pkglist | xargs`
        ;;
    Ubuntu)
        BASEPKGS=`cat $SETUPDIR/ubuntu.pkglist | xargs`
        MBPPKGS=''
        echo "Warning: No MBP $DISTRO support yet."
        ;;
    *)
        echo "Sorry, $DISTRO isn't supported."
        exit 1
        ;;
esac


split() {
    if [[ $1 == "" ]]; then
        1="()"
    else
        1="('$1')"
    fi

    python -c "import sys; sys.stdout.write('\n'.join([x for line in sys.stdin.readlines() for x in line.split$1]))"
}


function bootstrap {
    sudo mkdir -m777 -p /code

    case $DISTRO in
        Arch)
            echo "Installing pacaur..."
            sudo pacman -S --noconfirm pacaur

            echo "Downloading antigen.zsh"
            sudo mkdir -p /usr/share/zsh-antigen
            sudo curl -o /usr/share/zsh-antigen/antigen.zsh -sL git.io/antigen
            ;;
        Ubuntu)
            echo "Installing pacapt + symlinking to /usr/local/bin/pacman..."
            sudo wget -O /usr/local/bin/pacapt https://github.com/icy/pacapt/raw/ng/pacapt && \
                sudo chmod 755 /usr/local/bin/pacapt
            sudo ln -sv /usr/local/bin/pacapt /usr/local/bin/pacman || true
            sudo echo "sudo /usr/local/bin/pacapt" > /usr/local/bin/pacaur && \
                sudo chmod 755 /usr/local/bin/pacaur

            echo "Adding PPAs..."
            sudo add-apt-repository ppa:kgilmer/speed-ricer
            sudo add-apt-repository ppa:kelleyk/emacs
            sudo apt-get update

            echo "Installing kitty..."
            curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin
            ln -s $HOME/.local/kitty.app/bin/kitty $HOME/.local/bin/
           ;;
        *) ;;
    esac
}

function base_packages {
    echo "Installing base packages..."
    pacaur -S --noconfirm $BASEPKGS

    diff-so-fancy --set-defaults
}

function mbp_packages {
    mkdir /code/ext && cd /code/ext

    git clone https://github.com/aunali1/linux-mbp-arch.git && \
        cd linux-mbp-arch && \
        makepkg --skipinteg -si

    pacaur -S --noconfirm $MBPPKGS

    # Install BCE/touchbar/keyboard modules
    sudo git clone --branch mbp15 https://github.com/roadrunner2/macbook12-spi-driver.git /usr/src/apple-ibridge-0.1
    sudo dkms install -m apple-ibridge -v 0.1

    mods=("apple-bce" "apple-ib-tb" "apple-ib-als")
    for mod in "${arr[@]}"; do
        sudo modprobe $mod
        sudo echo $mod >> /etc/modules-load.d/apple.conf

    # Setup pulseaudio
    # https://gist.github.com/MCMrARM/c357291e4e5c18894bea10665dcebffb
    sudo cp ${$SETUPDIR/mbp,}/usr/share/alsa/cards/AppleT2.conf
    sudo cp ${$SETUPDIR/mbp,}/usr/share/pulseaudio/alsa-mixer/profile-sets/apple-t2.conf
    sudo cp ${$SETUPDIR/mbp,}/usr/lib/udev/rules.d/91-pulseaudio-custom.rules

    read -n 1 -p "Do you have a Macbook15,1? (y/n) " yn
    case $yn in
        y)
            sudo cp ${$SETUPDIR/mbp,}/lib/firmware/bcrm/* /lib/firmware/bcrm
            sudo modprobe -r bcrmfmac; sudo modprobe bcrmfmac
            sudo echo bcrmfmac >> /etc/modules-load.d/apple.conf

            sudo cat << EOF >> /etc/NetworkManager/NetworkManager.conf
[device]
wifi.backend=iwd
EOF
            sudo scre iwd
            sudo scre NetworkManager
        ;;
        *) echo "Skipping wifi driver installation" ;;
    esac
}

function pip_packages {
    sudo pip install -r $SETUPDIR/requirements.txt
}

function install_dotfiles {
    python $SETUPDIR/install.py
}

function activate_systemd {
    services=`ls $HOME/.config/systemd/user | grep -v wants | cut -d'.' -f1 | xargs`
    services="$services pulseaudio keybase keybase-redirector keybase-gui kbfs"
    echo "Enabling and starting systemd user services: $services"
    systemctl --user --now enable $services
}

function install_packages {
    base_packages

    case $MBPPKGS in
        "") yn="" ;;
        *) read -n 1 -p "Is this a Macbook Pro? (y/n)" yn ;;
    esac

    echo
    case $yn in
        y|Y) mbp_packages ;;
        *) echo "Skipping MBP packages" ;;
    esac

    pip_packages
    install_dotfiles
    compile_terminfo
}

function compile_terminfo {
    # Compiles xterm-24bit terminfo for emacs -nw
    tic -x -o $HOME/.terminfo $SETUPDIR/xterm-24bit.terminfo
}



function main {
    bootstrap
    install_packages
    activate_systemd
    xdg-settings set default-web-browser firefox.app
}

main
