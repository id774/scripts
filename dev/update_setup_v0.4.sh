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

# Last Setup
sudo vim /etc/hosts
#ifconfig | mail -s "[admin-log][`/bin/hostname`] cron started" xxxxxx@gmail.com
sudo vim /etc/init.d/cron

