#!/bin/sh
#
########################################################################
# pppconfig for emobile
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 7/26,2010
#       Minor fix.
#  v1.1 2/23,2010
#       Refactoring.
#  v1.0 8/15,2008
#       Stable.
########################################################################

test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts

sudo apt-get -y install pppconfig

# interfaces
sudo cp $SCRIPTS/etc/interfaces.static /etc/network/interfaces.static
sudo vi /etc/network/interfaces.static
sudo cp $SCRIPTS/etc/interfaces.dhcp /etc/network/interfaces.dhcp
sudo vi /etc/network/interfaces.dhcp
sudo cp /etc/network/interfaces.static /etc/network/interfaces

# emobile
sudo cp $SCRIPTS/etc/em /etc/ppp/peers/em
