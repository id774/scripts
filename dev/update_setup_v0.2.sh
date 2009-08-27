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

# Text Editor
sudo aptitude -y remove uim-el

# Deploy dot_emacs
$SCRIPTS/installer/install_dotemacs.sh

# Uninstall local build vim
#$SCRIPTS/installer/uninstall_vim.sh

# Deploy dot_files
$SCRIPTS/installer/install_dotfiles.sh dot_xmodmaprc_hhklite2

# Ruby
sudo aptitude -y install ruby1.8 ruby1.8-dev rubygems
#sudo aptitude -y install ruby1.9 ruby1.9-dev rubygems
$SCRIPTS/installer/install_ruby.sh 187-174
#$SCRIPTS/installer/install_ruby.sh 191-243
$SCRIPTS/installer/install_emacs_ruby.sh /usr/local/src/ruby/branches/ruby_1_8_7/misc
#$SCRIPTS/installer/install_emacs_ruby.sh /usr/local/src/ruby/branches/ruby_1_9_1/misc

# Crypt
$SCRIPTS/installer/install_des.sh
$SCRIPTS/installer/install_crypt.sh source
$SCRIPTS/installer/install_crypt.sh win
$SCRIPTS/installer/install_crypt.sh macosx
$SCRIPTS/installer/install_crypt.sh i386_deb

# RubyGems
$SCRIPTS/installer/install_rubygems.sh
$SCRIPTS/installer/install_gems.sh
$SCRIPTS/installer/install_rails.sh

# tune2fs
test -b /dev/sda5  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda5
test -b /dev/sda6  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda6
test -b /dev/sda7  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda7
test -b /dev/sda8  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda8
test -b /dev/sda9  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda9
test -b /dev/sda10 && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda10
test -b /dev/mapper/`/bin/hostname`-root && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname`-root
test -b /dev/mapper/`/bin/hostname`-tmp  && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname`-tmp
test -b /dev/mapper/`/bin/hostname`-var  && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname`-var
test -b /dev/mapper/`/bin/hostname`-usr  && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname`-usr
test -b /dev/mapper/`/bin/hostname`-home && sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname`-home

# Last Setup
# xfsuspend
which s2ram > /dev/null && which xflock4 > /dev/null && sudo cp $SCRIPTS/xfsuspend.sh /usr/local/sbin/xfsuspend && sudo chown root:root /usr/local/sbin/xfsuspend && sudo chmod 755 /usr/local/sbin/xfsuspend && sudo vim /usr/local/sbin/xfsuspend
# xfce4 custom themes
test -f /usr/share/themes/Xfce-dusk/gtk-2.0/gtkrc && sudo cp ~/scripts/etc/themes/xfce-dusk/gtkrc /usr/share/themes/Xfce-dusk/gtk-2.0/gtkrc
# sudoers
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

# Server Resource Report Job
sudo cp $SCRIPTS/get_resources.sh /root/bin/get_resources.sh
sudo chmod 700 /root/bin/get_resources.sh
sudo chown -R root:root /root/bin
# ClamAV Auto Upgrade Job
sudo cp $SCRIPTS/cron/bin/clamav_upgrade.sh /root/bin/clamav_upgrade.sh
sudo chmod 700 /root/bin/clamav_upgrade.sh
sudo cp $SCRIPTS/cron/bin/auto-upgrade /etc/cron.daily/auto-upgrade
sudo vim /etc/cron.daily/auto-upgrade
sudo chmod 750 /etc/cron.daily/auto-upgrade
sudo cp $SCRIPTS/cron/etc/clamscan_exclude /root/bin/clamscan_exclude
sudo vim /root/bin/clamscan_exclude
sudo chmod 600 /root/bin/clamscan_exclude
sudo chown -R root:root /root/bin

# Upgrade

# Debian unstable
#sudo aptitude -v full-upgrade && sudo aptitude autoclean

# Debian stable/testing
sudo aptitude -y safe-upgrade && sudo aptitude autoclean

# Ubuntu Server
#sudo aptitude -y safe-upgrade && sudo aptitude -y install linux-image-server linux-server gnupg && sudo aptitude autoclean

# Ubuntu Desktop
#sudo aptitude -y safe-upgrade && sudo aptitude -y install linux-image-generic linux-generic linux-restricted-modules-generic linux-headers linux-headers-generic gnupg && sudo aptitude autoclean
