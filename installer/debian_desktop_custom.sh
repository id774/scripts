#!/bin/sh
#
########################################################################
# Batch Custom Installers for Debian Desktop
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 9/28,2011
#       First version.
########################################################################

im-switch -c
sudo rmmod pcspkr
test -r /etc/modprobe.d/blacklist && sudo vi /etc/modprobe.d/blacklist
test -r /etc/modprobe.d/blacklist.conf && sudo vi /etc/modprobe.d/blacklist.conf

$SCRIPTS/installer/install_dotfiles.sh dot_xmodmaprc_lucid

which s2ram > /dev/null && which xflock4 > /dev/null && \
  sudo cp $SCRIPTS/xfsuspend.sh /usr/local/sbin/xfsuspend && \
  sudo chown root:root /usr/local/sbin/xfsuspend && \
  sudo chmod 755 /usr/local/sbin/xfsuspend && \
  sudo vi /usr/local/sbin/xfsuspend

test -f /usr/share/themes/Xfce-dusk/gtk-2.0/gtkrc && \
  sudo cp $HOME/scripts/etc/themes/xfce-dusk/gtkrc \
  /usr/share/themes/Xfce-dusk/gtk-2.0/gtkrc

# GDM Themes
$SCRIPTS/installer/install_gdmthemes.sh
$SCRIPTS/installer/install_gdmthemes2.sh

# Iceweasel and Icedove (Debian)
test -f /etc/lsb-release || $SCRIPTS/installer/install_iceweasel.sh

