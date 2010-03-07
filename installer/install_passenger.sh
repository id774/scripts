#!/bin/sh
#
########################################################################
# Install Passenger
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 3/7,2010
#       Refactoring.
#  v1.0 8/15,2008
#       Stable.
########################################################################

install_passenger() {
    sudo aptitude install -y g++ apache2-threaded-dev
    sudo touch /etc/apache2/mods-available/passenger.conf
    sudo touch /etc/apache2/mods-available/passenger.load

    cd /etc/apache2/mods-enabled
    sudo ln -s ../mods-available/passenger.conf
    sudo ln -s ../mods-available/passenger.load

    sudo mkdir -p /var/lib/rails
    sudo chown -R www-data:www-data /var/lib/rails
    sudo chmod -R g+rw,o-rwx /var/lib/rails

    sudo gem install passenger
    sudo passenger-install-apache2-module
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_passenger
