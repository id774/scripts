#!/bin/sh
#
# This scripts updates environment from 0.3 to 0.4
########################################################################

export SCRIPTS=$HOME/scripts

# APT Update
#DISTRIB_CODENAME=lenny
#SOURCESLIST=sources-$DISTRIB_CODENAME.list
#sudo cp $SCRIPTS/etc/$SOURCESLIST /etc/apt/sources.list
sudo vim /etc/apt/sources.list
sudo aptitude update

# sysklogd
sudo aptitude -y install klogd sysklogd

# Libraries and Programming Tools
sudo aptitude -y install scheme48 cmuscheme48-el

# Deploy dot_emacs
$SCRIPTS/installer/install_dotemacs.sh

# Server Resource Report Job
sudo cp $SCRIPTS/get_resources.sh /root/bin/get_resources.sh
sudo chmod 700 /root/bin/get_resources.sh
sudo chown -R root:root /root/bin

# Install plagger plugin
export plagger_dir=$TMP
test -d /usr/local/share/perl/5.8.8/Plagger && export plagger_dir=/usr/local/share/perl/5.8.8/Plagger
test -d /usr/local/share/perl/5.10.0/Plagger && export plagger_dir=/usr/local/share/perl/5.10.0/Plagger
sudo cp -Rv $SCRIPTS/cron/plagger/assets/plugins/* $plagger_dir/assets/plugins/
sudo cp -Rv $SCRIPTS/cron/plagger/plugins/publish/* $plagger_dir/Plugin/Publish/
sudo cp -Rv $SCRIPTS/cron/plagger/plugins/filter/* $plagger_dir/Plugin/Filter/
sudo cp -Rv $SCRIPTS/cron/plagger/plugins/customfeed/* $plagger_dir/Plugin/CustomFeed/

# Linux kernel source, headers, kbuild (Debian)
sudo aptitude -y purge linux-headers-2.6.26-1-686
sudo aptitude -y install linux-headers-2.6.26-2-686

# Last Setup
# /var/log/cron.log
sudo vim /etc/syslog.conf
# SYSLOGD="-m 0"
sudo vim /etc/default/syslogd

# Upgrade

# Debian unstable
#sudo aptitude -v full-upgrade && sudo aptitude autoclean

# Debian stable/testing
sudo aptitude -y safe-upgrade && sudo aptitude autoclean

# Ubuntu Server
#sudo aptitude -y safe-upgrade && sudo aptitude -y install linux-image-server linux-server gnupg && sudo aptitude autoclean

# Ubuntu Desktop
#sudo aptitude -y safe-upgrade && sudo aptitude -y install linux-image-generic linux-generic linux-restricted-modules-generic linux-headers linux-headers-generic gnupg && sudo aptitude autoclean
