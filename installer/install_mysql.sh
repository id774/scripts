#!/bin/sh
#
########################################################################
# Install MySQL Configuration
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

sudo /etc/init.d/mysql stop
sudo cp $SCRIPTS/etc/my-utf8.cnf /etc/mysql/my.cnf
sudo /etc/init.d/mysql start
