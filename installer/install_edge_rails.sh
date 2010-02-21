#!/bin/sh
#
########################################################################
# Install or Update Edge Rails
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 2/20,2010
#       Fix directory.
#  v1.0 9/8,2008
#       Stable.
########################################################################

update_rails() {
    cd /usr/local/src/rails/trunk/rails
    sudo git pull
}

install_rails() {
    test -d /usr/local/src/rails/trunk || sudo mkdir -p /usr/local/src/rails/trunk
    cd /usr/local/src/rails/trunk
    sudo git clone git://github.com/rails/rails.git
}

case $OSTYPE in
  *darwin*)
    OWNER=root:wheel
    ;;
  *)
    OWNER=root:root
    ;;
esac

test -d /usr/local/src/rails/trunk/rails && update_rails
test -d /usr/local/src/rails/trunk/rails || install_rails
sudo chown -R $OWNER /usr/local/src/rails/trunk

