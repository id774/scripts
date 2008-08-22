#!/bin/sh
#
########################################################################
# Install GDM Themes
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

test -d /usr/share/gdm/themes || exit 0
wget http://big.freett.com/railsinstall2/gdmthemes.tar.gz
sudo tar xzvf gdmthemes.tar.gz -C /usr/share/gdm/themes
sudo chown root:root -R /usr/share/gdm/themes
rm gdmthemes.tar.gz
