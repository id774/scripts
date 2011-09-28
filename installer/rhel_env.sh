#!/bin/sh
#
########################################################################
# Environment for RHEL
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 9/26,2011
#       First version.
########################################################################

show_hw_info() {
    cat /proc/meminfo
    cat /proc/cpuinfo
}

clear_udev() {
    sudo rm /etc/udev/rules.d/70-persistent-net.rules
}

operation() {
    show_hw_info
    clear_udev

    # Network
    $SCRIPTS/installer/rhel_network.sh

    # tune2fs
    $SCRIPTS/installer/setup_tune2fs.sh

    # SELinux
    sudo vi /etc/sysconfig/selinux

    # Upgrade
    sudo yum -y update
}

operation
