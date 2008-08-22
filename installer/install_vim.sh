#!/bin/sh
#
########################################################################
# Install Vim
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

mkdir install_vim
cd install_vim
wget ftp://ftp.vim.org/pub/vim/unix/vim-7.1.tar.bz2
wget ftp://ftp.vim.org/pub/vim/extra/vim-7.1-extra.tar.gz
wget ftp://ftp.vim.org/pub/vim/extra/vim-7.1-lang.tar.gz
tar xjvf vim-7.1.tar.bz2
tar xzvf vim-7.1-extra.tar.gz
tar xzvf vim-7.1-lang.tar.gz
mkdir patches
cd patches
zsh -c "wget ftp://ftp.vim.org/pub/vim/patches/7.1/7.1.{001..330}"
cd ../
test -d /usr/local/src/vim && sudo rm -rf /usr/local/src/vim
test -d /usr/local/src/vim || sudo mkdir -p /usr/local/src/vim
sudo cp vim-7.1.tar.bz2 /usr/local/src/vim
sudo cp vim-7.1-extra.tar.gz /usr/local/src/vim
sudo cp vim-7.1-lang.tar.gz /usr/local/src/vim
sudo cp -a vim71 /usr/local/src/vim
sudo cp -a patches /usr/local/src/vim
sudo chown -R root:root /usr/local/src/vim
cd vim71
cat ../patches/7.1.* | patch -p0
./configure --enable-multibyte --enable-xim --enable-fontset --with-features=big
make
sudo make install
cd ../../
rm -rf install_vim
