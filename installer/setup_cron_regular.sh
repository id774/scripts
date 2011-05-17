#!/bin/sh
#
########################################################################
# Setup for cron.regular scripts
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 5/16,2011
#       First.
########################################################################

export SCRIPTS=$HOME/scripts

# SetUp
sudo cp $SCRIPTS/etc/cron.regular /etc/cron.regular
sudo chown root:root /etc/cron.regular
sudo chmod 755 /etc/cron.regular
test -d /etc/cron.regular.d || sudo mkdir -p /etc/cron.regular.d
sudo chown root:root /etc/cron.regular.d
sudo chmod 755 /etc/cron.regular.d
