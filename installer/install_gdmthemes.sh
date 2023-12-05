#!/bin/sh
#
########################################################################
# Install GDM Themes
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 2011-09-30
#       Relocation tar file.
#  v1.0 2008-08-15
#       Stable.
########################################################################

test -d /usr/share/gdm/themes || exit 0
wget http://id774.net/archive/gdmthemes.tar.gz
sudo tar xzvf gdmthemes.tar.gz -C /usr/share/gdm/themes
sudo chown root:root -R /usr/share/gdm/themes
rm gdmthemes.tar.gz
