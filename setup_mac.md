# 10.14 Mac Setup #

## Prerequisites ##
* [iTerm2](https://iterm2.com/downloads.html)

## [Homebrew](https://brew.sh/) ##
```bash
# Installation
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Packages
brew install gpg npm python3 the_silver_searcher wemux

# Casks
brew cask install emacs macdown
```

## /code + [dotfiles](https://github.com/gmoben/dotfiles) ##
```bash
sudo mkdir /code
sudo chmod -R 777 /code
mkdir /code/ben
cd /code/ben && git checkout --recursive https://github.com/gmoben/dotfiles && cd dotfiles
./install.sh
```

## Manual installation ##

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
