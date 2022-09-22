#!/bin/sh

sudo chown -R $(whoami) /usr/local/share/zsh /usr/local/share/zsh/site-functions

NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/uninstall.sh)"

test -d /usr/local/Cellar && sudo rm -rf /usr/local/Cellar
test -d /usr/local/Homebrew && sudo rm -rf /usr/local/Homebrew

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

$SCRIPTS/installer/install_brew_batch.sh

sudo chown -R root:wheel /usr/local/share/zsh /usr/local/Homebrew/completions/zsh

