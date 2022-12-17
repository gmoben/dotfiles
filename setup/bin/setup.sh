#!/usr/bin/bash

set -o errexit
set -o pipefail
shopt -s extglob

ROOT=$(cd $(dirname $0)/../.. && pwd -P)
SETUP=$ROOT/setup

source $ROOT/.shutils
source $SETUP/bin/utils.sh

function set_distro {
    case "$1" in
    *Microsoft*)
        if [[ `command -v lsb_release` ]]; then
            set_distro `lsb_release -a 2>/dev/null | grep 'Distributor ID' | awk '{ print $3 }'`
        else
        set_distro unsupported
        fi;;
    *arch*|*MANJARO*|*microsoft*)
        DISTRO=arch
        INSTALL_CMD='pacaur -S --noconfirm --noedit --needed';;
    *Ubuntu*)
        DISTRO=ubuntu
        INSTALL_CMD='sudo apt install -y';;
    *Darwin*)
        DISTRO=darwin
        INSTALL_CMD='brew install';;
    *armv7l*)
        DISTRO=rpi
        INSTALL_CMD='sudo apt install -y';;
    *)
        DISTRO=unsupported
        error "Unsupported platform $(bold $PLATFORM) detected"
        exit 1;;
    esac
}


PLATFORM=$(eval python3 -mplatform | grep -iE 'arch|Ubuntu|MANJARO|Microsoft|armv7l')
set_distro $PLATFORM
success "Platform Detected" "$PLATFORM"
success "Distribution Detected" "$DISTRO"


yesno "Is this a Macbook (15,1)?" && IS_MBP=0 || IS_MBP=1

function install_pkg {
    local usage="Usage: install_pkg PKGNAME"
    1=${1?usage}
    $INSTALL_CMD $1
}

function install_antidote {
    info "Downloading antidote..."
    git clone --depth=1 https://github.com/mattmc3/antidote.git ${ZDOTDIR:-$HOME}/.antidote
}


function bootstrap {
    mkdir -m755 -p $HOME/code

    sudo ln -s $HOME/code /code

    mkdir -p /code/ben && \
        mkdir -p /code/ext && \
        mkdir -p $HOME/.local/bin

    touch /code/.ruby-version

    case $DISTRO in
        arch)
            info "Installing yay..."
            sudo pacman -S --noconfirm --needed yay
            info "Installing pacaur..."
            pacaur -S --noconfirm --needed pacaur
            ;;
        ubuntu)
            info "Adding PPAs"
            for src in $(cat $SETUP/ubuntu/apt.ppa); do
                sudo add-apt-repository -y ppa:$src
            done

            info "Updating package sources"
            sudo apt-get update

            info "Installing kitty from source"
            curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin

            [[ -e $HOME/.local/bin/kitty ]] && rm -f $HOME/.local/bin/kitty
            ln -s $HOME/.local/kitty.app/bin/kitty $HOME/.local/bin/
            ;;
        rpi)
            sudo apt-get update
            ;;
        *) warning "No bootstrap step for distro" "$DISTRO" ;;
    esac
}

function install_mbp_extras {
    info 'Installing MBP extras'

    if [[ $IS_MBP -eq 0 ]]; then
    mods=("apple-bce" "apple-ib-tb" "apple-ib-als" "applesmc")
    for mod in "${mods[@]}"; do
            sudo modprobe $mod
            sudo echo $mod >> /etc/modules-load.d/modules.conf
    done

    # Setup pulseaudio
    # https://gist.github.com/MCMrARM/c357291e4e5c18894bea10665dcebffb
    sudo cp ${$SETUP/arch/mbp/root,}/usr/share/alsa/cards/AppleT2.conf
    sudo cp ${$SETUP/arch/mbp/root,}/usr/share/pulseaudio/alsa-mixer/profile-sets/apple-t2.conf
    sudo cp ${$SETUP/arch/mbp/root,}/usr/lib/udev/rules.d/91-pulseaudio-custom.rules

    yesno "Do you have a Macbook15,1?" && (
            sudo cp ${$SETUP/arch/mbp/root,}/lib/firmware/bcrm/* /lib/firmware/bcrm
            sudo modprobe -r bcrmfmac; sudo modprobe bcrmfmac
            sudo echo bcrmfmac >> /etc/modules-load.d/apple.conf
            sudo cat << EOF >> /etc/NetworkManager/NetworkManager.conf
[device]
wifi.backend=iwd
EOF
            sudo systemctl enable --now iwd && \
        systemctl restart NetworkManager && \
        systemctl enable --now NetworkManager
    ) || warning "Skipping wifi driver installation"
    fi

}

function pip_packages {
    local _pip=`which pip3`

    if [[ -n "$_pip" && ! -e "$(dirname $_pip)/pip" ]]; then
        sudo ln -s $_pip $(dirname $_pip)/pip
    elif [[ -z "$_pip" ]]; then
        _pip=`which pip`
    fi

    if [[ `command -v $_pip` ]]; then
        sudo $_pip install --ignore-installed -r $SETUP/python/requirements.txt
        $_pip install --user pipx
        pipx ensurepath
    else
        error "Can't find pip!"
        exit 1
    fi

    cat $SETUP/python/pipx.txt | while read line || [[ -n $line ]]; do
        pipx install $line
    done


}

function install_dotfiles {
    $SETUP/install_dotfiles.py
}

function activate_systemd {
    #sudo systemctl --now enable sshd || true
    #sudo systemctl --now enable NetworkManager || true
    services=`ls $HOME/.config/systemd/user | grep -v wants | cut -d'.' -f1 | xargs`
    services="$services pulseaudio keybase keybase-redirector" # keybase-gui kbfs"
    info "Enabling and starting systemd user services: $services" ""
    systemctl --user --now enable $services || true
}

function install_base {
    info "Installing base packages"

    case $DISTRO in
        arch)
            PKGLIST=`cat $SETUP/arch/pacaur.pkglist | grep -vE "#.*" | xargs`
            $INSTALL_CMD rustup
            rustup default stable
            rustup update
            mkdir -p $HOME/.zfunc
            ;;
        ubuntu)
            PKGLIST=`cat $SETUP/ubuntu/apt.pkglist | grep -vE "#.*" | xargs`
            [[ $IS_MBP -eq 1 ]] && warning "Warning: No MBP $DISTRO support yet."
            ;;
        rpi)
            PKGLIST=`cat $SETUP/rpi/apt.pkglist | grep -vE "#.*" | xargs`
            ;;
        *)
            error "Sorry, $DISTRO isn't supported."
            exit 1
            ;;
    esac

    $INSTALL_CMD $PKGLIST
}

function install_packages {

    install_base

    case $DISTRO in
    arch)
        [[ $IS_MBP -eq 0 ]] && install_mbp_extras
        ;;
    ubuntu)
        echo "Installing diff-so-fancy..."
        sudo npm install --global diff-so-fancy

        echo "Installing Keybase..."
        local deb="keybase_amd64.deb"
        local url="https://prerelease.keybase.io/$deb"
        $(cd /tmp && curl -O $url)
        $INSTALL_CMD /tmp/$deb
        rm -f /tmp/$deb
        run_keybase
        ;;
    rpi)
        echo "Installing AWSCLI v2 from Github..."

        $(cd /tmp && git clone https://github.com/aws/aws-cli.git \
              && cd aws-cli && git checkout v2 \
              && pip3 install -r requirements.txt \
              && pip3 install .)
        rm -rf /tmp/aws-cli
        ;;
    *)
        warning "No extras to install for distribution $DISTRO" " ";;
    esac

    diff-so-fancy --set-defaults || true
    pip_packages
}

function compile_terminfo {
    # Compiles xterm-24bit terminfo for emacs -nw
    sudo tic -x -o /usr/share/terminfo $SETUP/terminfo/xterm-24bit
}

function setup_git {
    declare -A gitconfig
    gitconfig[user.name]="gmoben"
    gitconfig[user.email]="ben@warr.io"

    LIBSECRET=/usr/share/git/credential/libsecret
    if [[ -d $LIBSECRET ]]; then
        echo "Setting up libsecret..."
        sudo make --directory=$LIBSECRET
        gitconfig[credential.helper]="$LIBSECRET/git-credential-libsecret"
    else
        warning "Missing git-credential-libsecret source at $LIBSECRET. Not adding to git config."
    fi

    for key in "${!gitconfig[@]}"; do
        git config --global $key "${gitconfig[$key]}" || echo "Setting git config option $key failed!"
    done

}

function main {
    bootstrap
    install_packages
    setup_git
    install_antidote || error "Failed antidote installation"
    install_dotfiles || error "Failed dotfile installation"
    compile_terminfo || error "Failed compiling xterm-24bit terminfo"
    systemctl status &>/dev/null && activate_systemd || warning "Systemctl probably disabled, skipping activation..."
    xdg-settings set default-web-browser "firefox.desktop" || error "Failed setting default web browser via xdg-settings"
}

function setup_rpi {
    compile_terminfo || error "Failed compiling xterm-24bit terminfo"
    $INSTALL_CMD
}

if [[ $1 == 'rpi' ]]; then
    setup_rpi
else
    main
fi
