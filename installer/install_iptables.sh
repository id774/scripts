#!/bin/sh
#
########################################################################
# Install iptables
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 9/27,2011
#       for RHEL.
#  v1.0 8/15,2008
#       Stable.
########################################################################

install_iptables() {
    case "$1" in
        rhel)
            sudo cp $SCRIPTS/etc/iptables-rhel /etc/sysconfig/iptables
            sudo chmod 600 /etc/sysconfig/iptables
            sudo chown root:root /etc/sysconfig/iptables
            ;;
        *)
            sudo cp $SCRIPTS/etc/iptables-deb /etc/network/if-pre-up.d/iptables
            sudo chmod 700 /etc/network/if-pre-up.d/iptables
            sudo chown root:root /etc/network/if-pre-up.d/iptables
            ;;
    esac
}

install_iptables $*
