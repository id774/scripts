#!/bin/sh
#
# This scripts updates environment from 0.2 to 0.3
########################################################################

export SCRIPTS=$HOME/scripts

# APT Update
#DISTRIB_CODENAME=lenny
#SOURCESLIST=sources-$DISTRIB_CODENAME.list
#sudo cp $SCRIPTS/etc/$SOURCESLIST /etc/apt/sources.list
sudo vim /etc/apt/sources.list
sudo aptitude update

# Libraries and Programming Tools
sudo aptitude -y install global

# Deploy dot_emacs
$SCRIPTS/installer/install_dotemacs.sh

# Uninstall local build vim
#$SCRIPTS/installer/uninstall_vim.sh

# Deploy dot_files
$SCRIPTS/installer/install_dotfiles.sh dot_xmodmaprc_hhklite2

# Ruby
$SCRIPTS/installer/install_ruby.sh 187-svn

# Last Setup
sudo cp $SCRIPTS/etc/sudoers /etc/sudoers
sudo vim /etc/sudoers

# Renew logrotate settings
test -f /etc/logrotate.d/backup          && sudo cp -v $SCRIPTS/cron/etc/backup-log          /etc/logrotate.d/backup
test -r /etc/logrotate.d/auto-upgrade    && sudo cp -v $SCRIPTS/cron/etc/auto-upgrade-log    /etc/logrotate.d/auto-upgrade
test -r /etc/logrotate.d/resources       && sudo cp -v $SCRIPTS/cron/etc/resources-log       /etc/logrotate.d/resources
test -r /etc/logrotate.d/clamav_upgrade  && sudo cp -v $SCRIPTS/cron/etc/clamav_upgrade-log  /etc/logrotate.d/clamav_upgrade
test -r /etc/logrotate.d/rsync_backup    && sudo cp -v $SCRIPTS/cron/etc/rsync_backup-log    /etc/logrotate.d/rsync_backup
test -r /etc/logrotate.d/plagger         && sudo cp -v $SCRIPTS/cron/etc/plagger-log         /etc/logrotate.d/plagger
test -r /etc/logrotate.d/mixi            && sudo cp -v $SCRIPTS/cron/etc/mixi-log            /etc/logrotate.d/mixi
test -r /etc/logrotate.d/clone_git2svn   && sudo cp -v $SCRIPTS/cron/etc/clone_git2svn-log   /etc/logrotate.d/clone_git2svn
test -r /etc/logrotate.d/clone_coderepos && sudo cp -v $SCRIPTS/cron/etc/clone_coderepos-log /etc/logrotate.d/clone_coderepos
ls -ltra /etc/logrotate.d/
sudo chmod 644 /etc/logrotate.d/*
sudo chown root:root /etc/logrotate.d/*

# Upgrade

# Debian unstable
#sudo aptitude -v full-upgrade && sudo aptitude autoclean

# Debian stable/testing
sudo aptitude -y safe-upgrade && sudo aptitude autoclean

# Ubuntu Server
#sudo aptitude -y safe-upgrade && sudo aptitude -y install linux-image-server linux-server gnupg && sudo aptitude autoclean

# Ubuntu Desktop
#sudo aptitude -y safe-upgrade && sudo aptitude -y install linux-image-generic linux-generic linux-restricted-modules-generic linux-headers linux-headers-generic gnupg && sudo aptitude autoclean
