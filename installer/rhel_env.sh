#!/bin/sh
#
########################################################################
# Environment for RHEL
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2011-09-26
#       First version.
########################################################################

show_hw_info() {
    cat /proc/meminfo
    cat /proc/cpuinfo
}

flash_udev_net_rules() {
    test -f /etc/udev/rules.d/70-persistent-net.rules && \
      sudo mv -f /etc/udev/rules.d/70-persistent-net.rules \
      /etc/udev/rules.d/70-persistent-net.rules.old
}

selinux_setup() {
    sudo vi /etc/sysconfig/selinux
    sudo chkconfig --level 2345 auditd on
}

operation() {
    show_hw_info
    #flash_udev_net_rules

    # Network
    $SCRIPTS/installer/rhel_network.sh

    # tune2fs
    $SCRIPTS/installer/setup_tune2fs.sh

    # SELinux
    selinux_setup

    # Upgrade
    sudo yum -y update
}

operation $*
