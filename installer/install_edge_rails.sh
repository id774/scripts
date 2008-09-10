#!/bin/sh
#
########################################################################
# Install or Update Edge Rails
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 9/8,2008
#       Stable.
########################################################################

update_rails() {
    cd /usr/local/src/rails
    sudo git pull
}

install_rails() {
    test -d /usr/local/src || sudo mkdir -p /usr/local/src
    cd /usr/local/src
    sudo git clone git://github.com/rails/rails.git
}

test -d /usr/local/src/rails && update_rails
test -d /usr/local/src/rails || install_rails

sudo chown -R root:root /usr/local/src/rails
