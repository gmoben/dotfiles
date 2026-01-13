#!/usr/bin/bash

set -o errexit
set -o pipefail
shopt -s extglob

ROOT=$(cd $(dirname $0)/../.. && pwd -P)
SETUP=$ROOT/setup

source $ROOT/.shutils
source $SETUP/bin/utils.sh

function set_distro {

    [[ "$1" == *microsoft* ]] && IS_WSL=0 || IS_WSL=1

    case "$1" in
    *Microsoft*)
        if [[ `command -v lsb_release` ]]; then
            set_distro `lsb_release -a 2>/dev/null | grep 'Distributor ID' | awk '{ print $3 }'`
        else
        set_distro unsupported
        fi;;
    *arch*|*MANJARO*|*microsoft*)
        DISTRO=arch
        INSTALL_CMD='yay -S --noconfirm --needed';;
    *Ubuntu*)
        DISTRO=ubuntu
        INSTALL_CMD='sudo apt install -y';;
    *Darwin*)
        DISTRO=darwin
        INSTALL_CMD='brew install';;
    *armv7l*)
        DISTRO=rpi
        INSTALL_CMD='sudo apt install -y';;
    *amzn2int*)
        DISTRO=al2
        INSTALL_CMD='sudo yum install -y';;
    *)
        DISTRO=unsupported
        error "Unsupported platform $(bold $PLATFORM) detected"
        exit 1;;
    esac
}


PLATFORM=$(eval python3 -mplatform | grep -iE 'arch|Ubuntu|MANJARO|Microsoft|armv7l|Linux')

# `python3 -mplatform` doesn't spit out Ubuntu on 22.04
if [[ $PLATFORM =~ ^Linux ]]; then
    if [[ $(uname -a) =~ .*Ubuntu.* ]]; then
    PLATFORM=Ubuntu
    fi
fi

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
    HOME_CODE=$HOME/code
    ROOT_CODE=/code

    mkdir -m755 -p $HOME_CODE

    if [ -d "$path" ] && [ "$(ls -A "$path")" ]; then
        error "$ROOT_CODE is a directory and is non-empty"
        exit 1
    elif [ -L "$ROOT_CODE" ]; then
        target=$(readlink -f "$ROOT_CODE")
        if [ "$target" != "$HOME_CODE" ]; then
            if yesno "$ROOT_CODE points to $target instead of $HOME_CODE. Remove?"; then
                info "Removing symlink at $ROOT_CODE"
                sudo rm -rf $ROOT_CODE
            else
                error "Not removing $ROOT_CODE. Exiting..."
                exit 1
            fi
        fi
    elif [ -e "$ROOT_CODE" ]; then
        error "File exists at $ROOT_CODE"
        exit 1
    else
        info "Creating symlink at $ROOT_CODE to $HOME_CODE"
        sudo ln -s $HOME_CODE $ROOT_CODE
    fi

    mkdir -p $HOME/code/ben && \
        mkdir -p $HOME/code/ext && \
        mkdir -p $HOME/.local/bin

    touch /code/.ruby-version

    mkdir -p $HOME/.zfunc

    case $DISTRO in
        arch)
            info "Installing yay..."
            sudo pacman -S --noconfirm --needed yay
            ;;
        ubuntu)
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

function install_system_configs {
    info 'Installing system configuration files'

    local root_dir="$SETUP/$DISTRO/root"
    if [[ ! -d "$root_dir" ]]; then
        info "No system configs found for $DISTRO at $root_dir"
        return 0
    fi

    # Copy all files from $DISTRO/root/ to / preserving directory structure
    cd "$root_dir"
    find . -type f | while read -r file; do
        local dest="/${file#./}"
        local dest_dir=$(dirname "$dest")
        sudo mkdir -p "$dest_dir"
        sudo cp "$root_dir/$file" "$dest"
        success "Installed" "$dest"
    done
    cd - > /dev/null
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

    if [[ ! `command -v $_pip` ]]; then
        error "Can't find pip!"
        exit 1
    fi

    case $DISTRO in
    arch)
        PKGLIST=`cat $SETUP/arch/python.pkglist | grep -vE "#.*" | xargs`
        $INSTALL_CMD $PKGLIST
        ;;
    *)
        sudo $_pip install --ignore-installed -r $SETUP/python/requirements.txt
        $_pip install --user pipx
        export PATH=$HOME/.local/bin:$PATH
        pipx ensurepath
        ;;
    esac

    cat $SETUP/python/pipx.txt | while read line || [[ -n $line ]]; do
        pipx install $line
    done

}

function install_dotfiles {
    $SETUP/install_dotfiles.py
}

function install_firefox_config {
    $SETUP/bin/firefox.sh
}

function activate_systemd {
    #sudo systemctl --now enable sshd || true
    sudo systemctl enable --now NetworkManager || true
    sudo systemctl enable --now bluetooth || true
    services=`ls $HOME/.config/systemd/user | grep -v wants | cut -d'.' -f1 | xargs`
    # services="$services node-hp-scan-to"
    info "Enabling and starting systemd user services: $services" ""
    systemctl --user --now enable $services || true
}

function add_to_ignorepkg {
    local pkg=$1
    local conf="/etc/pacman.conf"

    # Check if package is already in IgnorePkg
    if grep -qE "^IgnorePkg\s*=.*\b${pkg}\b" "$conf"; then
        info "Package $pkg already in IgnorePkg"
        return 0
    fi

    # Check if IgnorePkg line exists (uncommented)
    if grep -qE "^IgnorePkg\s*=" "$conf"; then
        # Append to existing IgnorePkg line
        sudo sed -i "s/^\(IgnorePkg\s*=.*\)/\1 ${pkg}/" "$conf"
    else
        # Uncomment and set IgnorePkg line, or add new one after the comment
        if grep -qE "^#IgnorePkg\s*=" "$conf"; then
            sudo sed -i "s/^#IgnorePkg\s*=.*/IgnorePkg   = ${pkg}/" "$conf"
        else
            # Add IgnorePkg line in the options section
            sudo sed -i "/^\[options\]/a IgnorePkg   = ${pkg}" "$conf"
        fi
    fi
    success "Added $pkg to IgnorePkg in $conf"
}

function install_with_patch {
    local pkg=$1
    local patch_file="$SETUP/arch/pkgbuild-patches/${pkg}.patch"
    local cache_dir="$HOME/.cache/yay/${pkg}"

    # Get patch modification time (Unix timestamp)
    local patch_mtime=$(stat -c %Y "$patch_file")

    # Query AUR for package last modified time
    local aur_mtime=$(curl -s "https://aur.archlinux.org/rpc/?v=5&type=info&arg=${pkg}" | \
        python3 -c "import sys,json; print(json.load(sys.stdin)['results'][0]['LastModified'])" 2>/dev/null || echo "0")

    if [[ "$patch_mtime" -gt "$aur_mtime" ]]; then
        info "Patch for $pkg is newer than AUR version"
        if yesno "Apply local patch for $pkg?"; then
            rm -rf "$cache_dir"
            git clone "https://aur.archlinux.org/${pkg}.git" "$cache_dir"
            cd "$cache_dir"
            patch -p1 < "$patch_file"
            makepkg -si --noconfirm
            cd -
            add_to_ignorepkg "$pkg"
            return 0
        fi
    else
        warning "Patch for $pkg is older than AUR version - AUR may have been updated"
        if yesno "Apply local patch anyway?"; then
            rm -rf "$cache_dir"
            git clone "https://aur.archlinux.org/${pkg}.git" "$cache_dir"
            cd "$cache_dir"
            patch -p1 < "$patch_file"
            makepkg -si --noconfirm
            cd -
            add_to_ignorepkg "$pkg"
            return 0
        fi
    fi

    # Fall back to normal yay install
    yay -S --noconfirm --needed "$pkg"
}

function install_base {
    info "Installing base packages"

    case $DISTRO in
        arch)
            # Filter out packages that have patches, install those separately
            local patched_pkgs=()
            PKGLIST=""
            for pkg in $(cat $SETUP/arch/yay.pkglist | grep -vE "#.*"); do
                if [[ -f "$SETUP/arch/pkgbuild-patches/${pkg}.patch" ]]; then
                    patched_pkgs+=("$pkg")
                else
                    PKGLIST="$PKGLIST $pkg"
                fi
            done

            # Install patched packages
            for pkg in "${patched_pkgs[@]}"; do
                install_with_patch "$pkg"
            done

            $INSTALL_CMD rustup
            rustup default nightly
            rustup update
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
        echo "Setting permissions to run `light` without sudo"
        sudo usermod -a -G video $USER
        sudo chmod +s /usr/bin/light

        echo "Removing tmux and downloading, compiling, and replacing with tmux $TMUX_VERSION"
        export TMUX_VERSION=3.4
        sudo apt remove -y tmux
        cd /code/ext
        wget https://github.com/tmux/tmux/releases/download/$TMUX_VERSION/tmux-$TMUX_VERSION.tar.gz
        tar -xvzf tmux-${TMUX_VERSION}.tar.gz
        cd tmux-${TMUX_VERSION}
        ./configure && make
        sudo make install
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
    cargo_packages

    if [[ $DISTRO != arch ]]; then
        git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
        ~/.fzf/install
    fi
}

function cargo_packages {
    cargo install ripgrep
    cargo install --locked bat
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

function setup_rpi {
    compile_terminfo || error "Failed compiling xterm-24bit terminfo"
    $INSTALL_CMD
}

function install_mise {
    if [[ ! -f $HOME/.local/bin/mise ]]; then
        curl https://mise.run | sh
        eval "$($HOME/.local/bin/mise activate --shims)"
    fi

    local plugins=("awsls" "bat" "bat-extras" "usage" "node" "python")

    for plugin in "${plugins[@]}"; do
        mise use -g $plugin
    done
}

function main {
    bootstrap
    install_packages
    install_system_configs
    setup_git
    install_mise || error "Failed mise installation"
    install_antidote || error "Failed antidote installation"
    install_dotfiles || error "Failed dotfile installation"
    install_firefox_config || warning "Firefox config setup skipped"
    compile_terminfo || error "Failed compiling xterm-24bit terminfo"
    systemctl status &>/dev/null && activate_systemd || warning "Systemctl probably disabled, skipping activation..."
    if [[ ! $IS_WSL ]]; then
        xdg-settings set default-web-browser "firefox.desktop" || error "Failed setting default web browser via xdg-settings"
    fi
}

if [[ $1 == 'rpi' ]]; then
    setup_rpi
else
    main
fi
