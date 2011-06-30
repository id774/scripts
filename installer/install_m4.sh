#!/bin/sh
#
########################################################################
# Install m4
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 6/30,2011
#       First.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=1.4.16
    test -n "$1" && VERSION=$1
}

save_sources() {
    sudo mkdir -p /usr/local/src/m4
    sudo cp -av m4-$VERSION /usr/local/src/m4/
    sudo chown -R root:root /usr/local/src/m4
}

install_m4() {
    setup_environment $*
    mkdir install_m4
    cd install_m4
    wget ftp://ftp.gnu.org/gnu/m4/m4-$VERSION.tar.gz
    tar xzvf m4-$VERSION.tar.gz
    cd m4-$VERSION
    ./configure --prefix=/usr/local
    make
    sudo make install
    cd ..
    test -n "$2" || save_sources
    cd ..
    rm -rf install_m4
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_m4 $*
