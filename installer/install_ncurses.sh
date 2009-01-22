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

mkdir ncurses57
cd ncurses57
wget http://ftp.gnu.org/pub/gnu/ncurses/ncurses-5.7.tar.gz
tar xzvf ncurses-5.7.tar.gz
cd ncurses-5.7
./configure --with-shared --with-normal
make
sudo make install
cd ../
test -d /usr/local/src/ncurses || sudo mkdir -p /usr/local/src/ncurses
sudo cp -a ncurses-5.7 /usr/local/src/ncurses
cd ../
rm -rf ncurses57
