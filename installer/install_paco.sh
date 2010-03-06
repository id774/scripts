#!/bin/sh
#
########################################################################
# Install paco
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 12/15,2008
#       Keep sources.
#  v1.0 12/4,2008
#       Stable.
########################################################################

install_paco() {
    PACO_VERSION=paco-2.0.6

    mkdir install_paco
    cd install_paco

    wget http://downloads.sourceforge.net/paco/$PACO_VERSION.tar.gz
    tar xzvf $PACO_VERSION.tar.gz
    cd $PACO_VERSION
    ./configure --disable-gpaco
    make
    sudo make install
    sudo make logme
    cd ..
    sudo mkdir -p /usr/local/src/paco
    sudo cp -av $PACO_VERSION /usr/local/src/paco/
    sudo chown -R root:root /usr/local/src/paco

    cd ..
    rm -rf install_paco
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_paco
