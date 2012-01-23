#!/bin/sh
#
########################################################################
# Install Node.js
#  $1 = version
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.5 1/23,2012
#       Update default version to v0.6.8.
#  v0.4 1/22,2012
#       Update default version to v0.6.6.
#  v0.3 12/8,2011
#       Update to v0.6.5.
#  v0.2 12/1,2011
#       Add npm.
#  v0.1 11/22,2011
#       First.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=0.6.8
    test -n "$1" && VERSION=$1
    NODE=node-v$VERSION
}

save_sources() {
    sudo mkdir -p /usr/local/src/node.js
    sudo cp -av $NODE /usr/local/src/node.js
    sudo chown -R root:root /usr/local/src/node.js
}

install_npm() {
    sudo sh -c 'curl http://npmjs.org/install.sh | sh'
}

install_node() {
    setup_environment $*

    mkdir install_node
    cd install_node

    wget http://nodejs.org/dist/v$VERSION/$NODE.tar.gz
    tar xzvf $NODE.tar.gz
    cd $NODE
    ./configure
    make
    sudo make install
    cd ..
    test -n "$2" || save_sources
    cd ..
    rm -rf install_node
    install_npm $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_node $*
