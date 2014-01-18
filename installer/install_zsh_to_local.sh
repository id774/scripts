#!/bin/sh

ZSH_VERSION=5.0.5
test -d $HOME/local/zsh || mkdir -p $HOME/local/zsh
$HOME/scripts/installer/install_zsh.sh $ZSH_VERSION $HOME/local/zsh/$ZSH_VERSION nosudo
$HOME/dot_zsh/install_dotzsh.sh $HOME/.zsh nosudo
cp $HOME/dot_zsh/dot_zshrc $HOME/.zshrc

