#!/usr/bin/bash

set -o errexit
set -o pipefail

SCRIPT_DIR=$(cd $(dirname $0) && pwd -P)
ROOT=$(cd $SCRIPT_DIR/../.. && pwd -P)

source $SCRIPT_DIR/utils.sh

FIREFOX_DIR="$HOME/.mozilla/firefox"
PROFILES_INI="$FIREFOX_DIR/profiles.ini"
SRC_DIR="$ROOT/.mozilla/firefox/profile"

# Find the default Firefox profile directory
find_default_profile() {
    if [[ ! -f "$PROFILES_INI" ]]; then
        return 1
    fi

    # Modern Firefox: look for [Install*] section's Default= value
    local install_default=$(grep -A1 '^\[Install' "$PROFILES_INI" 2>/dev/null | grep '^Default=' | head -1 | cut -d= -f2)
    if [[ -n "$install_default" && -d "$FIREFOX_DIR/$install_default" ]]; then
        echo "$FIREFOX_DIR/$install_default"
        return 0
    fi

    # Legacy: find [Profile*] with Default=1 and get its Path
    local in_profile=0
    local current_path=""
    local is_default=0

    while IFS= read -r line; do
        if [[ "$line" =~ ^\[Profile ]]; then
            # Check previous profile
            if [[ $is_default -eq 1 && -n "$current_path" ]]; then
                echo "$FIREFOX_DIR/$current_path"
                return 0
            fi
            in_profile=1
            current_path=""
            is_default=0
        elif [[ $in_profile -eq 1 ]]; then
            if [[ "$line" =~ ^Path= ]]; then
                current_path="${line#Path=}"
            elif [[ "$line" =~ ^Default=1 ]]; then
                is_default=1
            fi
        fi
    done < "$PROFILES_INI"

    # Check last profile
    if [[ $is_default -eq 1 && -n "$current_path" ]]; then
        echo "$FIREFOX_DIR/$current_path"
        return 0
    fi

    # Fallback: glob for *.default-release
    local default_release=$(ls -d "$FIREFOX_DIR"/*.default-release 2>/dev/null | head -1)
    if [[ -n "$default_release" ]]; then
        echo "$default_release"
        return 0
    fi

    return 1
}

# Symlink a file, handling existing files
symlink_file() {
    local src="$1"
    local dest="$2"

    if [[ ! -f "$src" ]]; then
        error "Source file missing" "$src"
        return 1
    fi

    # Create parent directory if needed
    local dest_dir=$(dirname "$dest")
    if [[ ! -d "$dest_dir" ]]; then
        info "Creating directory" "$dest_dir"
        mkdir -p "$dest_dir"
    fi

    if [[ -L "$dest" ]]; then
        local current_target=$(readlink "$dest")
        if [[ "$current_target" == "$src" ]]; then
            info "Already linked" "$dest"
            return 0
        fi
        info "Removing existing symlink" "$dest"
        rm "$dest"
    elif [[ -f "$dest" ]]; then
        if yesno "File exists at $dest. Replace with symlink?"; then
            rm "$dest"
        else
            warning "Skipping" "$dest"
            return 0
        fi
    fi

    info "Symlinking" "$src -> $dest"
    ln -s "$src" "$dest"
    success "Linked" "$dest"
}

main() {
    info "Firefox Config" "Setting up Firefox userChrome.css"

    # Check if Firefox has been run
    if [[ ! -d "$FIREFOX_DIR" ]]; then
        warning "Firefox directory not found" "$FIREFOX_DIR"
        warning "Run Firefox at least once to create a profile, then re-run this script."
        return 0
    fi

    # Find default profile
    local profile_dir
    profile_dir=$(find_default_profile)
    if [[ $? -ne 0 || -z "$profile_dir" ]]; then
        warning "Could not find Firefox profile"
        warning "Run Firefox at least once to create a profile, then re-run this script."
        return 0
    fi

    info "Found profile" "$profile_dir"

    # Symlink user.js
    symlink_file "$SRC_DIR/user.js" "$profile_dir/user.js"

    # Symlink chrome/userChrome.css
    symlink_file "$SRC_DIR/chrome/userChrome.css" "$profile_dir/chrome/userChrome.css"

    success "Firefox Config" "Complete! Restart Firefox for changes to take effect."
}

main "$@"
