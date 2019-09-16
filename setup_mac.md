# 10.14 Mac Setup #

## Prerequisites ##
* [iTerm2](https://iterm2.com/downloads.html)

## [Homebrew](https://brew.sh/) ##
```bash
# Installation
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Packages
brew install gpg npm python3 the_silver_searcher wemux yarn
brew install ghc cabal-install haskell-stack

# Casks
brew cask install emacs macdown
```

## /code ##
```bash
# Create /code with correct permissions
sudo mkdir /code
sudo chmod -R 777 /code
mkdir /code/ben
mkdir /code/go
```

## [dotfiles](https://github.com/gmoben/dotfiles) ##
```bash
cd /code/ben && git checkout --recursive https://github.com/gmoben/dotfiles && cd dotfiles
./install.sh
source ~/.profile
```

## Toolkits ##
```bash
# Rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup update

# GHCup
curl https://get-ghcup.haskell.org -sSf | sh

# dap-mode
pip install "ptvsd>=4.2"

# lsp-mode
pip3 install 'python-language-server[all]'
rustup component add rls rust-analysis rust-src

```

## Non-cask apps ##

* [Lignon](https://www.peterborgapps.com/lingon/) -- Set up a launch agent for `emacs --daemon`
* [Amethyst](https://github.com/ianyh/Amethyst)
* [Karabiner](https://pqrs.org/osx/karabiner/)
* [Docker](https://hub.docker.com/editions/community/docker-ce-desktop-mac)
* [Audacity](https://www.macupdate.com/app/mac/8052/audacity)

## Manual configuration ##

* `iTerm2`
  * Navigate to `Preferences > General > Preferences`
  * Select `Load preferences from a customer folder or URL` and enter `/code/ben/dotfiles`
  * Select `Save changes to folder when iTerm2 quits`
