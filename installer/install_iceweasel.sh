#!/bin/sh
#
########################################################################
# Install Iceweasel and Icedove
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 3/7,2010
#       Refactoring.
#  v1.1 10/22,2008
#       Choice x-www-browser for icedove.
#  v1.0 8/15,2008
#       Stable.
########################################################################

install_iceweasel() {
    sudo apt-get -y install iceweasel iceweasel-gnome-support iceweasel-l10n-ja
    sudo apt-get -y install icedove icedove-gnome-support icedove-locale-ja
    sudo update-alternatives --config x-www-browser
    sudo apt-get purge epiphany-gecko epiphany-extensions epiphany-browser
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_iceweasel
