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
wget ftp://ftp.vim.org/pub/vim/unix/vim-7.2.tar.bz2
wget ftp://ftp.vim.org/pub/vim/extra/vim-7.2-extra.tar.gz
wget ftp://ftp.vim.org/pub/vim/extra/vim-7.2-lang.tar.gz
tar xjvf vim-7.2.tar.bz2
tar xzvf vim-7.2-extra.tar.gz
tar xzvf vim-7.2-lang.tar.gz
mkdir patches
cd patches
curl -O 'ftp://ftp.vim.org/pub/vim/patches/7.2/7.2.[001-025]'
cd ../
test -d /usr/local/src/vim && sudo rm -rf /usr/local/src/vim
test -d /usr/local/src/vim || sudo mkdir -p /usr/local/src/vim
sudo cp vim-7.2.tar.bz2 /usr/local/src/vim
sudo cp vim-7.2-extra.tar.gz /usr/local/src/vim
sudo cp vim-7.2-lang.tar.gz /usr/local/src/vim
sudo cp -a vim72 /usr/local/src/vim
sudo cp -a patches /usr/local/src/vim
cd vim72
cat ../patches/7.2.* | patch -p0
./configure --enable-multibyte --enable-xim --enable-fontset --with-features=big --enable-perlinterp --enable-rubyinterp --enable-pythoninterp
make
sudo make install
cd ../../
rm -rf install_vim

case $OSTYPE in
  *darwin*)
    sudo chown -R root:wheel /usr/local/src/vim
    ;;
  *)
    sudo chown -R root:root /usr/local/src/vim
    ;;
esac

vim --version
