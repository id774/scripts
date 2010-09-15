#!/bin/sh
#
########################################################################
# Install ncurses
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 3/7,2010
#       Refactoring.
#  v1.0 8/15,2008
#       Stable.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=5.7
    test -n "$1" && VERSION=$1
    test -n "$2" || SUDO=
    test -n "$2" && SUDO=sudo
}

save_sources() {
    test -d /usr/local/src/ncurses || sudo mkdir -p /usr/local/src/ncurses
    sudo cp -a ncurses-$VERSION /usr/local/src/ncurses
}

install_ncurses() {
    setup_environment $*
    mkdir install_ncurses
    cd install_ncurses
    wget http://ftp.gnu.org/pub/gnu/ncurses/ncurses-$VERSION.tar.gz
    tar xzvf ncurses-$VERSION.tar.gz
    cd ncurses-$VERSION
    test -n "$2" || ./configure --with-shared --with-normal --prefix=$HOME/local/ncurses/$VERSION
    test -n "$2" && ./configure --with-shared --with-normal --prefix=$2
    make
    $SUDO make install
    cd ../
    save_sources
    cd ../
    rm -rf install_ncurses
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_ncurses $*
