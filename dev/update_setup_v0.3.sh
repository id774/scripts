#!/bin/sh
#
# This scripts updates environment from 0.3 to 0.4
########################################################################

export SCRIPTS=$HOME/scripts

# chromium-daily
#sudo apt-key adv --recv-keys --keyserver keyserver.ubuntu.com 0xfbef0d696de1c72ba5a835fe5a9bf3bb4e5e17b5

# APT Update
#DISTRIB_CODENAME=lenny
#SOURCESLIST=sources-$DISTRIB_CODENAME.list
#sudo cp $SCRIPTS/etc/$SOURCESLIST /etc/apt/sources.list
sudo vim /etc/apt/sources.list
sudo aptitude update

# chromium-daily
#sudo aptitude install chromium-browser

# Make Directory
test -d /opt/sbin || sudo mkdir /opt/sbin
test -d /opt/bin || sudo mkdir /opt/bin

# SSH, Compiler, Shell etc..
sudo aptitude -y install lha

# Libraries and Programming Tools
sudo aptitude -y install sysvconfig
sudo aptitude -y install libxslt-dev libxslt-ruby python-libxslt1

# Debian Developer Tools
if [ `aptitude search debget | awk '/^i/' | wc -l` = 0 ]; then
    sudo aptitude -y install dpkg-dev lintian debhelper yada equivs cvs-buildpackage dupload fakeroot devscripts debget
fi
if [ `aptitude search apt-listbugs | awk '/^i/' | wc -l` = 0 ]; then
    sudo aptitude -y install apt-listchanges apt-listbugs
fi
sudo vim /etc/apt/apt.conf.d/10apt-listbugs*

# sysklogd
if [ `aptitude search sysklogd | awk '/^i/' | wc -l` = 0 ]; then
    sudo aptitude -y install klogd sysklogd
    sudo vim /etc/syslog.conf
    sudo vim /etc/default/syslogd
fi

# Libraries and Programming Tools
if [ `aptitude search clisp | awk '/^i/' | wc -l` = 0 ]; then
    sudo aptitude -y install clisp
fi
if [ `aptitude search scheme48 | awk '/^i/' | wc -l` = 0 ]; then
    sudo aptitude -y install scheme48 cmuscheme48-el
fi

# makeinfo for emacs byte compling
if [ `aptitude search texinfo | awk '/^i/' | wc -l` = 0 ]; then
    sudo aptitude -y install texinfo
fi

# Deploy dot_emacs
$SCRIPTS/installer/install_dotemacs.sh

# navi2ch
#$SCRIPTS/installer/install_navi2ch.sh

# paco
$SCRIPTS/installer/install_paco.sh

# Deploy dot_files
$SCRIPTS/installer/install_dotfiles.sh

# Purge old ruby
test -d /usr/local/src/ruby/ruby-1.8.7-p248 && sudo rm -rf /usr/local/src/ruby/ruby-1.8*
test -d /usr/local/src/ruby/ruby-1.8.7-p174 && sudo rm -rf /usr/local/src/ruby/ruby-1.8*
test -d /usr/local/src/ruby/ruby-1.8.7-p72  && sudo rm -rf /usr/local/src/ruby/ruby-1.8*
# Ruby
sudo aptitude -y install libopenssl-ruby
sudo aptitude -y install libreadline-dev
#$SCRIPTS/installer/install_ruby.sh 187-svn
#$SCRIPTS/installer/install_ruby.sh 187-299 /opt/ruby/1.8.7
$SCRIPTS/installer/install_ruby.sh 191-429 /opt/ruby/1.9.1
$SCRIPTS/config/update-alternatives-ruby.sh

# Python
#test -d /usr/local/src/python/Python-* && sudo rm -rf /usr/local/src/python/Python-*
#$SCRIPTS/installer/install_python.sh 2.6.5 /opt/python/2.6.5
#$SCRIPTS/installer/install_python.sh 3.1.2 /opt/python/3.1.2
#$SCRIPTS/config/update-alternatives-python.sh

# Web Application Framework
$SCRIPTS/installer/install_django.sh 1.1.1
$SCRIPTS/installer/install_django.sh

# RubyGems
$SCRIPTS/installer/install_rubygems.sh 137 /opt/ruby/1.9.1
$SCRIPTS/installer/install_gems.sh /opt/ruby/1.9.1
$SCRIPTS/installer/install_rails.sh rails-ruby19 /opt/ruby/1.9.1
vim-ruby-install.rb

# Passenger
$SCRIPTS/installer/install_passenger.sh /opt/ruby/1.9.1

# cronjob
sudo cp $SCRIPTS/get_resources.sh /root/bin/get_resources.sh
sudo chmod 700 /root/bin/get_resources.sh
sudo cp $SCRIPTS/cron/etc/backup_exclude /root/bin/backup_exclude
sudo vim /root/bin/backup_exclude
sudo chmod 600 /root/bin/backup_exclude
sudo cp $SCRIPTS/cron/bin/clamav_upgrade.sh /root/bin/clamav_upgrade.sh
sudo chmod 700 /root/bin/clamav_upgrade.sh
sudo cp $SCRIPTS/cron/etc/clamscan_exclude /root/bin/clamscan_exclude
sudo vim /root/bin/clamscan_exclude
sudo chmod 600 /root/bin/clamscan_exclude
sudo chown -R root:root /root/bin
test -f /etc/cron.monthly/clamav_upgrade && sudo mv -v /etc/cron.monthly/clamav_upgrade /etc/cron.weekly/
sudo cp $SCRIPTS/cron/etc/clamav_upgrade-log /etc/logrotate.d/clamav_upgrade
sudo chmod 644 /etc/logrotate.d/clamav_upgrade
sudo chown root:root /etc/logrotate.d/clamav_upgrade

# Install plagger plugin
$SCRIPTS/installer/install_plagger_plugins.sh

# Linux kernel source, headers, kbuild (Debian)
sudo aptitude -y purge linux-headers-2.6.26-1-686
sudo aptitude -y install linux-headers-2.6.26-2-686

# Add path to root shell and crontab
sudo vim /etc/profile
sudo vim /etc/crontab

# Disable su
sudo vim /etc/pam.d/su

# Upgrade

# Debian unstable
#sudo aptitude -v full-upgrade && sudo aptitude autoclean

# Debian stable/testing
sudo aptitude -y safe-upgrade && sudo aptitude autoclean

# Ubuntu Server
#sudo aptitude -y safe-upgrade && sudo aptitude -y install linux-image-server linux-server gnupg && sudo aptitude autoclean

# Ubuntu Desktop
#sudo aptitude -y safe-upgrade && sudo aptitude -y install linux-image-generic linux-generic linux-restricted-modules-generic linux-headers linux-headers-generic gnupg && sudo aptitude autoclean
