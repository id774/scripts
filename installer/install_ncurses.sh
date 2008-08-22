#!/bin/sh
#
########################################################################
# Install ncurses
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

mkdir ncurses56
cd ncurses56
wget http://ftp.gnu.org/pub/gnu/ncurses/ncurses-5.6.tar.gz
tar xzvf ncurses-5.6.tar.gz
cd ncurses-5.6
./configure --with-shared --with-normal
make
sudo make install
cd ../
test -d /usr/local/src/ncurses || sudo mkdir -p /usr/local/src/ncurses
sudo cp -a ncurses-5.6 /usr/local/src/ncurses
cd ../
rm -rf ncurses56
