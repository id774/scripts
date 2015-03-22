#!/bin/sh
#
########################################################################
# Install TA-Lib
#  $1 = version
#  $2 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 3/23,2015
#       First.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=0.4.0
    test -n "$1" && VERSION=$1
    FILENAME=ta-lib-$VERSION-src.tar.gz
}

save_sources() {
    sudo mkdir -p /usr/local/src
    sudo cp -av ta-lib /usr/local/src/
    sudo chown -R root:root /usr/local/src/
}

install_talib() {
    setup_environment $*

    mkdir install_talib
    cd install_talib

    curl -L http://prdownloads.sourceforge.net/ta-lib/$FILENAME -O
    tar xzvf $FILENAME
    cd ta-lib
    ./configure --prefix=/usr/local
    make
    sudo make install
    cd ..
    test -n "$2" || save_sources
    cd ..
    rm -rf install_talib
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_talib $*
