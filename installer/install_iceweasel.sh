#!/bin/sh
#
########################################################################
# Install Iceweasel and Icedove
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 10/22,2008
#       Choice x-www-browser for icedove.
#  v1.0 8/15,2008
#       Stable.
########################################################################

sudo aptitude -y install iceweasel iceweasel-gnome-support iceweasel-l10n-ja
sudo aptitude -y install icedove icedove-gnome-support icedove-locale-ja

sudo update-alternatives --config x-www-browser
sudo aptitude purge epiphany-gecko epiphany-extensions epiphany-browser

