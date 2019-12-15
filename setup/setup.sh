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

    # # Patch apple-bce-dkms-git AUR package
    # # Ensure KVERSION is `uname -r` and remove non-DKMS target
    # git clone https://aur.archlinux.org/apple-bce-git.git && \
        #     cd apple-bce-git && \
        #     sed -i -r 's/(_kernver=|KVERSION=)\S+/\1$(uname -r)/' PKGBUILD && \
        #     cat PKGBUILD | python -c "import sys, re; sys.stdout.write(re.sub(r'package_apple-bce-git().+?}$\n\n', '', sys.stdin.read(), flags=re.M|re.S))" > PKGBUILD && \
        #     makepkg -si

    pacaur -S --noconfirm $MBPPKGS

    sudo git clone --branch mbp15 https://github.com/roadrunner2/macbook12-spi-driver.git /usr/src/apple-ibridge-0.1
    sudo dkms install -m apple-ibridge -v 0.1
    sudo modprobe apple-ib-tb
    sudo modprobe apple-ib-als

    cd /code/ext
    git clone https://github.com/MCMrARM/mbp2018-etc.git
    cd mbp2018-etc/applesmc
    make
    sudo insmod applesmc_t2_kmod.ko
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
    systemctl --user enable $services
    systemctl --user start $services
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
}


function main {
    bootstrap
    install_packages
    activate_systemd
}

main
