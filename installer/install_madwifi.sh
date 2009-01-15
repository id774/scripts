#!/bin/sh
#
########################################################################
# Install MadWifi
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 1/15,2009
#       Stable.
########################################################################

set_network_permission() {
    case $OSTYPE in
      *darwin*)
        sudo chown -R root:wheel /usr/local/src/network
        ;;
      *)
        sudo chown -R root:root /usr/local/src/network
        ;;
    esac
}

install_trunk() {
    test -d /usr/local/src/network/madwifi || sudo mkdir -p /usr/local/src/network/madwifi
    cd /usr/local/src/network/madwifi
    sudo svn co http://svn.madwifi-project.org/madwifi/trunk
    cd trunk
    sudo make
    sudo make install
    sudo modinfo ath_pci
    sudo vim /etc/modules # add line "ath_pci"
}

install_trunk
set_network_permission

