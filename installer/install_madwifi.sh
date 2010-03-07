#!/bin/sh
#
########################################################################
# Install MadWifi
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 3/7,2010
#       Refactoring.
#  v1.0 1/15,2009
#       Stable.
########################################################################

setup_environment() {
    case $OSTYPE in
      *darwin*)
        OWNER=root:root
        ;;
      *)
        OWNER=root:wheel
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
    sudo vim /etc/modules # add line "ath_pci"
    sudo chown -R $OWNER /usr/local/src/network
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_trunk
