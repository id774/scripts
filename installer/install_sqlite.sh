#!/bin/sh
#
########################################################################
# Install paco
#  $1 = version
#  $2 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 2011-06-08
#       Fix typo.
#  v0.1 2010-09-26
#       First.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=3.7.2
    test -n "$1" && VERSION=$1
}

save_sources() {
    sudo mkdir -p /usr/local/src/sqlite
    sudo cp -av sqlite-$VERSION /usr/local/src/sqlite/
    sudo chown -R root:root /usr/local/src/sqlite
}

install_sqlite() {
    setup_environment $*
    mkdir install_sqlite
    cd install_sqlite
    wget http://www.sqlite.org/sqlite-$VERSION.tar.gz
    tar xzvf sqlite-$VERSION.tar.gz
    cd sqlite-$VERSION
    ./configure --disable-tcl
    make
    sudo make install
    cd ..
    test -n "$2" || save_sources
    cd ..
    rm -rf install_sqlite
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_sqlite $*
