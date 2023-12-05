#!/bin/sh
#
########################################################################
# Install GDM Themes 2
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 2011-09-30
#       Relocation tar file.
#  v1.0 2008-11-17
#       Stable.
########################################################################

test -d /usr/share/gdm/themes || exit 0
wget http://id774.net/archive/gdmthemes2.tar.gz
sudo tar xzvf gdmthemes2.tar.gz -C /usr/share/gdm/themes
sudo chown root:root -R /usr/share/gdm/themes
rm gdmthemes2.tar.gz
