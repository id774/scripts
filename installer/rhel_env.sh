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

setup_group_and_passwd() {
    sudo vi /etc/group
    sudo vi /etc/passwd
    sudo passwd root
}

clear_udev() {
    sudo rm /etc/udev/rules.d/70-persistent-net.rules
}

operation() {
    show_hw_info

    # Network
    $SCRIPTS/installer/rhel_network.sh

    clear_udev

    # tune2fs
    $SCRIPTS/installer/setup_tune2fs.sh

    # Upgrade
    sudo yum -y update

    setup_group_and_passwd

    # Setup sudoers
    sudo vi /etc/sudoers $SCRIPTS/etc/sudoers
}

operation
