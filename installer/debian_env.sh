#!/bin/sh
#
########################################################################
# Environment for Debian
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
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

clear_udev() {
    sudo rm /etc/udev/rules.d/70-persistent-net.rules
}

setup_apt_source() {
    SOURCESLIST=sources-$DISTRIB_CODENAME.list
    sudo cp $SCRIPTS/etc/$SOURCESLIST /etc/apt/sources.list
    sudo vi /etc/apt/sources.list
    sudo apt-get update
}

ubuntu_ja_gpg() {
    eval `cat /etc/lsb-release`
    wget -q http://www.ubuntulinux.jp/ubuntu-ja-archive-keyring.gpg -O- \
      | sudo apt-key add -
    sudo wget http://www.ubuntulinux.jp/sources.list.d/$DISTRIB_CODENAME.list -O \
      /etc/apt/sources.list.d/ubuntu-ja.list
    sudo apt-get update
}

chromium_daily_gpg() {
    sudo apt-key adv --recv-keys --keyserver keyserver.ubuntu.com \
      0xfbef0d696de1c72ba5a835fe5a9bf3bb4e5e17b5
}

stop_services() {
    sudo update-rc.d -f cupsys remove
    sudo update-rc.d -f hplip remove
    sudo apt-get remove apt-index-watcher
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
    clear_udev

    # tune2fs
    $SCRIPTS/installer/setup_tune2fs.sh

    test -f /etc/lsb-release && DISTRIB_CODENAME=lucid
    test -f /etc/lsb-release || DISTRIB_CODENAME=squeeze
    setup_apt_source
    #ubuntu_ja_gpg
    #chromium_daily_gpg

    stop_services
    opt_bin_and_sbin
    home_permission
}

operation
