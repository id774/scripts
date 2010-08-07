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
$SCRIPTS/installer/install_rclocal.sh

# Last Setup
sudo vim /etc/hosts
sudo vim /etc/init.d/cron
# Disable motd
sudo vim /etc/pam.d/sshd
sudo vim /etc/pam.d/login

