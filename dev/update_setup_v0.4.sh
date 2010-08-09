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

# Network
$SCRIPTS/installer/install_pppconfig.sh

# Install cronjob
$SCRIPTS/installer/install_cronjob.sh

# Crypt
$SCRIPTS/installer/install_crypt.sh src
$SCRIPTS/installer/install_crypt.sh win
$SCRIPTS/installer/install_crypt.sh mac
#$SCRIPTS/installer/install_crypt.sh linux-i386
#$SCRIPTS/installer/install_crypt.sh linux-amd64

# Deploy dot_emacs
$SCRIPTS/installer/install_dotemacs.sh

# Termtter
$PRIVATE/installer/install_dottermtter.sh
$SCRIPTS/installer/install_termtter_plugins.sh

# Install plagger plugin
$SCRIPTS/installer/install_plagger_plugins.sh

# Mail to admin when startup
$SCRIPTS/installer/install_rclocal.sh

# Permissions for /src
sudo chown -R root:root /usr/src
sudo chown -R root:root /usr/local/src

# Last Setup
sudo vim /etc/hosts
sudo vim /etc/init.d/cron
# Disable motd
sudo vim /etc/pam.d/sshd
sudo vim /etc/pam.d/login

