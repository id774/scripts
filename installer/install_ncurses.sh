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

install_ncurses() {
    VERSION=5.7
    mkdir install_ncurses
    cd install_ncurses
    wget http://ftp.gnu.org/pub/gnu/ncurses/ncurses-$VERSION.tar.gz
    tar xzvf ncurses-$VERSION.tar.gz
    cd ncurses-$VERSION
    ./configure --with-shared --with-normal
    make
    sudo make install
    cd ../
    test -d /usr/local/src/ncurses || sudo mkdir -p /usr/local/src/ncurses
    sudo cp -a ncurses-$VERSION /usr/local/src/ncurses
    cd ../
    rm -rf install_ncurses
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_ncurses
