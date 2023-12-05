#!/bin/sh
#
########################################################################
# Install IP Messenger for Linux
#  $1 = version
#  $2 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.3 2010-09-16
#       Refactoring.
#  v1.2 2010-03-07
#       Refactoring and update to 0.9.6.
#  v1.1 2008-12-15
#       Set permission of source.
#  v1.0 2008-08-15
#       Stable.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=0.9.6
    test -n "$1" && VERSION=$1
}

save_sources() {
    sudo mkdir -p /usr/local/src/ipmsg
    sudo cp -av $IPMSG /usr/local/src/ipmsg/
    sudo chown -R root:root /usr/local/src/ipmsg
}

install_ipmsg() {
    setup_environment $*
    IPMSG=g2ipmsg-$VERSION

    sudo apt-get -y install libgtk2.0-dev libgnomeui-dev libpanelappletmm-2.6-dev

    mkdir install_ipmsg
    cd install_ipmsg

    wget http://www.ipmsg.org/archive/$IPMSG.tar.gz
    tar xzvf $IPMSG.tar.gz
    cd $IPMSG
    ./configure
    make
    sudo make install
    cd ..
    test -n "$2" || save_sources
    cd ..
    rm -rf install_ipmsg
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_ipmsg $*
