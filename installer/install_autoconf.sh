#!/bin/sh
#
########################################################################
# Install autoconf
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 4/26,2011
#       First.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=2.68
    test -n "$1" && VERSION=$1
}

save_sources() {
    sudo mkdir -p /usr/local/src/autoconf
    sudo cp -av autoconf-$VERSION /usr/local/src/autoconf/
    sudo chown -R root:root /usr/local/src/autoconf
}

install_autoconf() {
    setup_environment $*
    mkdir install_autoconf
    cd install_autoconf
    wget ftp://ftp.gnu.org/gnu/autoconf/autoconf-$VERSION.tar.gz
    tar xzvf autoconf-$VERSION.tar.gz
    cd autoconf-$VERSION
    ./configure --prefix=/usr
    make
    sudo make install
    cd ..
    test -n "$2" || save_sources
    cd ..
    rm -rf install_autoconf
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_autoconf $*
