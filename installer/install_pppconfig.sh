#!/bin/sh
#
########################################################################
# E-mobile on Linux (Debian/Ubuntu) setup script
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 2/23,2010
#       Refactoring.
#  v1.0 8/15,2008
#       Stable.
########################################################################

if [ `aptitude search pppconfig | awk '/^i/' | wc -l` = 0 ]; then
    sudo aptitude -y install pppconfig
fi

sudo cp $SCRIPTS/etc/interfaces.static /etc/network/interfaces.static
sudo vim /etc/network/interfaces.static
sudo cp $SCRIPTS/etc/interfaces.dhcp /etc/network/interfaces.dhcp
sudo vim /etc/network/interfaces.dhcp
sudo cp /etc/network/interfaces.dhcp /etc/network/interfaces

sudo cp $SCRIPTS/etc/em /etc/ppp/peers/em

pon em
