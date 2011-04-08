#!/bin/sh
#
########################################################################
# RHEL/CentOS Batch Network Configuration
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 4/8,2011
#       First.
########################################################################

# redhat?
test -f /etc/redhat-release || exit 1

# eth0
sudo vim /etc/sysconfig/network-scripts/ifcfg-eth0

# hostname
sudo vim /etc/sysconfig/network

# dns
sudo vim /etc/resolv.conf

# service restart
sudo service network restart

# show settings
/sbin/ifconfig
/bin/netstat -rn
