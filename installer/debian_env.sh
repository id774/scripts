#!/bin/sh
#
########################################################################
# Environment for Debian
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 4/16,2012
#       Shape up unnecessary functions.
#  v0.1 9/28,2011
#       First version.
########################################################################

show_hw_info() {
    cat /proc/meminfo
    cat /proc/cpuinfo
}

create_admin_group() {
    sudo groupadd admin
    sudo groupadd wheel
}

setup_apt_source() {
    SOURCESLIST=sources-$DISTRIB_CODENAME.list
    sudo cp $SCRIPTS/etc/$SOURCESLIST /etc/apt/sources.list
    sudo vi /etc/apt/sources.list
    sudo apt-get update
}

chromium_daily_gpg() {
    sudo apt-key adv --recv-keys --keyserver keyserver.ubuntu.com \
      0xfbef0d696de1c72ba5a835fe5a9bf3bb4e5e17b5
}

opt_bin_and_sbin() {
    sudo mkdir -p /opt/sbin
    sudo mkdir -p /opt/bin
}

home_permission() {
    sudo chmod 750 /home/*
}

operation() {
    show_hw_info
    create_admin_group

    # tune2fs
    $SCRIPTS/installer/setup_tune2fs.sh

    test -f /etc/lsb-release && DISTRIB_CODENAME=trusty
    test -f /etc/lsb-release || DISTRIB_CODENAME=jessie
    setup_apt_source
    #chromium_daily_gpg

    opt_bin_and_sbin
    home_permission
}

operation $*
