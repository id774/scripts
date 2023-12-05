#!/bin/sh
#
########################################################################
# Install Numix
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 2016-12-26
#       Running on Ubuntu.
#  v0.1 2016-12-24
#       First version.
########################################################################

UBUNTU_CODENAME=xenial
DEBIAN_CODENAME=jessie

# if not Debian, exit 2
test -f /etc/debian_version || exit 2

# Already installed, exit 1
test -f /etc/apt/sources.list.d/numix-ppa-$UBUNTU_CODENAME.list && exit 1
test -f /etc/apt/sources.list.d/numix-ppa-$DEBIAN_CODENAME.list && exit 1

sudo apt-get install -y software-properties-common
sudo add-apt-repository ppa:numix/ppa

# If Debian, Replace source list from Debian to Ubuntu
test -f /etc/apt/sources.list.d/numix-ppa-$DEBIAN_CODENAME.list && sudo sed -i "s/$DEBIAN_CODENAME/$UBUNTU_CODENAME/g" /etc/apt/sources.list.d/numix-ppa-$DEBIAN_CODENAME.list
sudo apt-get update

sudo apt-get install -y numix-gtk-theme numix-icon-theme-circle
