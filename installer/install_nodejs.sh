#!/bin/sh
#
########################################################################
# Install Node.js
#  $1 = version
#  $2 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 2013-05-22
#       Moved npmjs.org.
#  v1.1 2013-04-07
#       Add meteor.
#  v1.0 2012-06-26
#       Stable.
#  v0.1 2011-11-22
#       First.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=0.10.13
    test -n "$1" && VERSION=$1
    NODE=node-v$VERSION
}

save_sources() {
    sudo mkdir -p /usr/local/src/node.js
    sudo cp -av $NODE /usr/local/src/node.js
    sudo chown -R root:root /usr/local/src/node.js
}

install_npm() {
    sudo sh -c 'curl https://npmjs.org/install.sh | sh'
}

install_meteor() {
    sudo sh -c 'curl http://install.meteor.com | sh'
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
    install_meteor $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_node $*
