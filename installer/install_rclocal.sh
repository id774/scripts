#!/bin/sh
#
########################################################################
# Setup for rc.local scripts
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 8/6,2010
#       Stable.
########################################################################

export SCRIPTS=$HOME/scripts

sudo cp $SCRIPTS/etc/rc.local /etc/rc.local
sudo chown root:root /etc/rc.local
sudo chmod 755 /etc/rc.local
sudo cp $SCRIPTS/etc/rc.local.mail_to_admin /etc/rc.local.mail_to_admin
sudo vim /etc/rc.local.mail_to_admin /etc/rc.local
sudo chown root:adm /etc/rc.local.mail_to_admin
sudo chmod 740 /etc/rc.local.mail_to_admin
sudo cp $SCRIPTS/etc/rc.local.connect_to_emobile /etc/rc.local.connect_to_emobile
sudo chown root:root /etc/rc.local.connect_to_emobile
sudo chmod 755 /etc/rc.local.connect_to_emobile

