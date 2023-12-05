#!/bin/sh
#
########################################################################
# Install Resin
#  $1 = version
#  $2 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 2011-06-22
#       Version added to installation target.
#  v0.1 2011-06-16
#       First.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=4.0.19
    test -n "$1" && VERSION=$1
    RESIN=resin-$VERSION
}

save_sources() {
    sudo mkdir -p /usr/local/src/resin
    sudo cp -av $RESIN /usr/local/src/resin/
    sudo chown -R root:root /usr/local/src/resin
}

install_resin() {
    setup_environment $*

    mkdir install_resin
    cd install_resin

    wget http://www.caucho.com/download/$RESIN.zip
    unzip $RESIN.zip
    cd $RESIN
    ./configure --prefix=/opt/resin/$VERSION
    make
    sudo make install
    cd ..
    test -n "$2" || save_sources
    cd ..
    rm -rf install_resin
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_resin $*
