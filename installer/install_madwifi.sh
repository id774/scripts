#!/bin/sh
#
########################################################################
# Install MadWifi
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 2010-03-07
#       Refactoring.
#  v1.0 2009-01-15
#       Stable.
########################################################################

setup_environment() {
    case $OSTYPE in
      *darwin*)
        OWNER=root:wheel
        ;;
      *)
        OWNER=root:root
        ;;
    esac
}

install_trunk() {
    setup_environment
    test -d /usr/local/src/network/madwifi || sudo mkdir -p /usr/local/src/network/madwifi
    cd /usr/local/src/network/madwifi
    sudo svn co http://svn.madwifi-project.org/madwifi/trunk
    cd trunk
    sudo make
    sudo make install
    sudo modinfo ath_pci
    sudo vi /etc/modules # add line "ath_pci"
    sudo chown -R $OWNER /usr/local/src/network
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_trunk
