#!/bin/sh
#
########################################################################
# Install libyaml
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 1/24,2011
#       First.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=0.1.4
    test -n "$1" && VERSION=$1
}

save_sources() {
    sudo mkdir -p /usr/local/src/libyaml
    sudo cp -av yaml-$VERSION /usr/local/src/libyaml/
    sudo chown -R root:root /usr/local/src/libyaml
}

install_libyaml() {
    setup_environment $*
    mkdir install_libyaml
    cd install_libyaml
    wget http://pyyaml.org/download/libyaml/yaml-$VERSION.tar.gz
    tar xzvf yaml-$VERSION.tar.gz
    cd yaml-$VERSION
    ./configure --prefix=/usr/local
    make
    sudo make install
    cd ..
    test -n "$2" || save_sources
    cd ..
    rm -rf install_libyaml
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_libyaml $*
