#!/bin/sh
#
########################################################################
# Setup for rc.local scripts
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 2011-09-29
#       Flash udev net persistent rules.
#  v0.2 2010-08-07
#       Make directory rc.local.d.
#  v0.1 2010-08-06
#       Stable.
########################################################################

test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts

# SetUp
sudo cp $SCRIPTS/etc/rc.local /etc/rc.local
sudo chown root:root /etc/rc.local
sudo chmod 755 /etc/rc.local
test -d /etc/rc.local.d || sudo mkdir -p /etc/rc.local.d
sudo chown root:root /etc/rc.local.d
sudo chmod 755 /etc/rc.local.d

sudo cp $SCRIPTS/etc/rc.local.d/* /etc/rc.local.d/

sudo chown root:root /etc/rc.local.d/*
sudo chmod 755 /etc/rc.local.d/*

sudo chown root:adm /etc/rc.local.d/mail_to_admin
sudo chmod 740 /etc/rc.local.d/mail_to_admin
sudo vi /etc/rc.local.d/mail_to_admin
