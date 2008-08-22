#!/bin/sh
#
########################################################################
# Install iptables
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

sudo cp $SCRIPTS/etc/iptables /etc/network/if-pre-up.d/iptables
sudo chmod 700 /etc/network/if-pre-up.d/iptables
sudo chown root:root /etc/network/if-pre-up.d/iptables
sudo vim /etc/network/if-pre-up.d/iptables
