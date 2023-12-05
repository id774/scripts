#!/bin/sh
#
########################################################################
# Install Iceweasel and Icedove
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 2010-03-07
#       Refactoring.
#  v1.1 2008-10-22
#       Choice x-www-browser for icedove.
#  v1.0 2008-08-15
#       Stable.
########################################################################

install_iceweasel() {
    sudo apt-get -y install iceweasel iceweasel-gnome-support iceweasel-l10n-ja
    sudo apt-get -y install icedove icedove-gnome-support icedove-locale-ja
    sudo update-alternatives --config x-www-browser
    sudo apt-get purge epiphany-gecko epiphany-extensions epiphany-browser
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_iceweasel
