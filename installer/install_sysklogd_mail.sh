#!/bin/sh
#
########################################################################
# Install sysklogd logger (for Ubuntu Hardy)
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

sudo cp $SCRIPTS/etc/sysklogd /etc/init.d/sysklogd
sudo chmod 750 /etc/init.d/sysklogd
sudo chown root:adm /etc/init.d/sysklogd
sudo vi /etc/init.d/sysklogd

