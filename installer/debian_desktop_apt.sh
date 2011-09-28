#!/bin/sh
#
########################################################################
# Bulk Apt Install Script for Debian Desktop
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 9/28,2011
#       Forked from Initial Setup Script.
########################################################################

export SCRIPTS=$HOME/scripts

# GUI Desktop Xfce4(Debian) / Xubuntu(Ubuntu)
test -f /etc/lsb-release && sudo apt-get -y install xubuntu-desktop
test -f /etc/lsb-release || sudo apt-get -y install xfce4
sudo apt-get -y install xfwm4 xfwm4-themes
sudo apt-get -y install xfce4-goodies

# Ubuntu-ja
test -f /etc/lsb-release && sudo apt-get -y install ubuntu-desktop-ja

# Fonts
sudo apt-get -y install xfonts-mplus
sudo apt-get -y install xfonts-shinonome
sudo apt-get -y install ttf-vlgothic ttf-bitstream-vera

# Codec
test -f /etc/lsb-release && sudo apt-get -y install ubuntu-restricted-extras
test -f /etc/lsb-release && sudo apt-get -y install xubuntu-restricted-extras

# Icons
test -f /etc/lsb-release && sudo apt-get -y install ubuntu-artwork xubuntu-artwork human-icon-theme
sudo apt-get -y install gnome-themes gnome-themes-extras

# OpenOffice.org
sudo apt-get -y install openoffice.org

# Mozilla Thunderbird (Ubuntu)
sudo apt-get -y install mozilla-thunderbird

# gthumb
sudo apt-get -y install gthumb

# thunar
sudo apt-get -y install thunar

# vlc
sudo apt-get -y install vlc

# pidgin
sudo apt-get -y install pidgin

# pdf
sudo apt-get -y install xpdf xpdf-reader

# 2ch Browser
sudo apt-get -y install ochusha
sudo apt-get -y install jd

# Comic Viewer
sudo apt-get -y install comix

# CD/DVD Creator
sudo apt-get -y install gnomebaker

# P2P
sudo apt-get -y install skype

# MSN
sudo apt-get -y install amsn

# Wireshark
sudo apt-get -y install wireshark

# chromium-daily
sudo apt-get -y install chromium-browser

