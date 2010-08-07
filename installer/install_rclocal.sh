#!/bin/sh
#
########################################################################
# Setup for rc.local scripts
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 8/7,2010
#       Make directory rc.local.d.
#  v0.1 8/6,2010
#       Stable.
########################################################################

export SCRIPTS=$HOME/scripts

# Delete old scripts
test -f /etc/rc.local.mail_to_admin && sudo rm /etc/rc.local.mail_to_admin
test -f /etc/rc.local.connect_to_emobile && sudo rm /etc/rc.local.connect_to_emobile

# SetUp
sudo cp $SCRIPTS/etc/rc.local /etc/rc.local
sudo chown root:root /etc/rc.local
sudo chmod 755 /etc/rc.local
test -d /etc/rc.local.d || sudo mkdir -p /etc/rc.local.d
sudo chown root:root /etc/rc.local.d
sudo chmod 755 /etc/rc.local.d

sudo cp $SCRIPTS/etc/rc.local.d/connect_to_emobile /etc/rc.local.d/
sudo chown root:root /etc/rc.local.d/connect_to_emobile
sudo chmod 755 /etc/rc.local.d/connect_to_emobile

sudo cp $SCRIPTS/etc/rc.local.d/mail_to_admin /etc/rc.local.d/
sudo chown root:adm /etc/rc.local.d/mail_to_admin
sudo chmod 740 /etc/rc.local.d/mail_to_admin
sudo vim /etc/rc.local.d/mail_to_admin
