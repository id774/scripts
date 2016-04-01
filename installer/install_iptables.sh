#!/bin/sh
#
########################################################################
# Install iptables
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 4/1,2016
#       Using iptables-persistent.
#  v1.1 9/27,2011
#       for RHEL.
#  v1.0 8/15,2008
#       Stable.
########################################################################

smart_apt() {
    while [ $# -gt 0 ]
    do
        if [ `aptitude search $1 | awk '/^i/' | wc -l` = 0 ]; then
            sudo apt-get -y install $1
        fi
        shift
    done
}

install_iptables() {
    case "$1" in
        rhel)
            sudo cp $SCRIPTS/etc/iptables/iptables-rhel /etc/sysconfig/iptables
            sudo chmod 600 /etc/sysconfig/iptables
            sudo chown root:root /etc/sysconfig/iptables
            ;;
        debian6)
            sudo cp $SCRIPTS/etc/iptables/iptables-deb /etc/network/if-pre-up.d/iptables
            sudo chmod 700 /etc/network/if-pre-up.d/iptables
            sudo chown root:root /etc/network/if-pre-up.d/iptables
            ;;
        *)
            test -f /etc/network/if-pre-up.d/iptables && sudo rm /etc/network/if-pre-up.d/iptables
            smart_apt iptables-persistent
            test -d /etc/iptables || sudo mkdir /etc/iptables
            sudo cp $SCRIPTS/etc/iptables/rules.v4 /etc/iptables/rules.v4
            sudo chmod 600 /etc/iptables/rules.v4
            sudo chmod 600 /etc/iptables/rules.v6
            sudo chown root:root /etc/iptables/rules.v4
            sudo chown root:root /etc/iptables/rules.v6
            sudo vim /etc/iptables/rules.v4
            sudo sh -c 'iptables-restore < /etc/iptables/rules.v4'
            ;;
    esac
}

install_iptables $*
