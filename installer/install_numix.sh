#!/bin/sh
#
########################################################################
# Install Numix for Debian
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 12/24,2016
#       First version.
########################################################################

# if Ubuntu, exit 1
test -f /etc/lsb-release && exit 1
# if not Debian, exit 1
test -f /etc/debian_version || exit 1

sudo apt-get install -y software-properties-common
sudo add-apt-repository ppa:numix/ppa

sudo sed -i 's/jessie/xenial/g' /etc/apt/sources.list.d/numix-ppa-jessie.list
sudo apt-get update

sudo apt-get install -y numix-gtk-theme numix-icon-theme-circle
