#!/bin/sh
#
########################################################################
# Install iptables
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 2016-04-01
#       Using iptables-persistent.
#  v1.1 2011-09-27
#       for RHEL.
#  v1.0 2008-08-15
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
            sudo vim /etc/sysconfig/iptables
            ;;
        debian6)
            sudo cp $SCRIPTS/etc/iptables/iptables-deb /etc/network/if-pre-up.d/iptables
            sudo chmod 700 /etc/network/if-pre-up.d/iptables
            sudo chown root:root /etc/network/if-pre-up.d/iptables
            sudo vim /etc/network/if-pre-up.d/iptables
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
