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

smart_apt() {
    while [ $# -gt 0 ]
    do
        if [ `aptitude search $1 | awk '/^i/' | wc -l` = 0 ]; then
            sudo apt-get -y install $1
        fi
        shift
    done 
}

desktop_envirionment() {
    test -f /etc/lsb-release && smart_apt xubuntu-desktop
    test -f /etc/lsb-release || smart_apt xfce4
    #test -f /etc/lsb-release && smart_apt ubuntu-desktop-ja
    smart_apt \
      xfwm4 xfwm4-themes \
      xfce4-goodies \
      gnome-themes gnome-themes-extras
}

fonts_packages() {
    smart_apt \
      xfonts-mplus \
      xfonts-shinonome \
      ttf-vlgothic ttf-bitstream-vera
}

codec_packages() {
    smart_apt \
      ubuntu-restricted-extras \
      xubuntu-restricted-extras
}

icon_packages() {
    smart_apt \
      ubuntu-artwork xubuntu-artwork human-icon-theme
}

optional_packages() {
    test -f /etc/lsb-release && smart_apt mozilla-thunderbird
    test -f /etc/lsb-release || smart_apt icedove
    smart_apt \
      uim \
      openoffice.org \
      gthumb \
      thunar \
      vlc \
      pidgin \
      xpdf xpdf-reader \
      comix \
      gnomebaker \
      skype \
      amsn \
      wireshark \
      xtightvncviewer \
      chromium-browser
}

increase_debian_packages() {
    desktop_envirionment
    fonts_packages
    test -f /etc/lsb-release && codec_packages
    test -f /etc/lsb-release && icon_packages
    optional_packages
}

operation() {
    test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
    test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts
    increase_debian_packages
}

operation $*
