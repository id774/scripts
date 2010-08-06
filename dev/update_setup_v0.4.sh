#!/bin/sh
#
# This scripts updates environment from 0.4 to 0.5
########################################################################

export SCRIPTS=$HOME/scripts

# APT Update
#DISTRIB_CODENAME=lucid
#SOURCESLIST=sources-$DISTRIB_CODENAME.list
#sudo cp $SCRIPTS/etc/$SOURCESLIST /etc/apt/sources.list
sudo vim /etc/apt/sources.list
sudo aptitude update

# Install cronjob
$SCRIPTS/installer/install_cronjob.sh

# Deploy dot_emacs
$SCRIPTS/installer/install_dotemacs.sh

# Install plagger plugin
$SCRIPTS/installer/install_plagger_plugins.sh

# Mail to admin when startup
sudo cp $SCRIPTS/etc/rc.local /etc/rc.local
sudo cp $SCRIPTS/etc/rc.local.mail_to_admin /etc/rc.local.mail_to_admin
sudo vim /etc/rc.local.mail_to_admin /etc/rc.local
sudo chown root:root /etc/rc.local
sudo chmod 755 /etc/rc.local
sudo chown root:adm /etc/rc.local.mail_to_admin
sudo chmod 740 /etc/rc.local.mail_to_admin

# Last Setup
sudo vim /etc/hosts
sudo vim /etc/init.d/cron

