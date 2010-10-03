#!/bin/sh
#
# This scripts updates environment from 0.4 to 0.5
# $1 = i386, or amd64
########################################################################

export SCRIPTS=$HOME/scripts

# Need i386 or amd64 option
test -n "$1" || exit 1

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

# Ruby
$SCRIPTS/installer/install_ruby.sh 192-svn /opt/ruby/1.9.2
test -d /usr/local/src/ruby/ruby-1.8.7-p299 && sudo rm -rf /usr/local/src/ruby/ruby*
$SCRIPTS/config/update-alternatives-ruby.sh

# RubyGems
$SCRIPTS/installer/install_gems.sh /opt/ruby/1.9.2
$SCRIPTS/installer/install_rails.sh rails-ruby /opt/ruby/1.9.2
/opt/ruby/1.9.2/bin/vim-ruby-install.rb

# Passenger
$SCRIPTS/installer/install_passenger.sh /opt/ruby/1.9.2
$SCRIPTS/config/update-alternatives-ruby.sh

# Python Framework
vim $SCRIPTS/installer/install_python_framework.sh
$SCRIPTS/installer/install_python_framework.sh

# Crypt
$SCRIPTS/installer/install_crypt.sh src 7.0a
$SCRIPTS/installer/install_crypt.sh win 7.0a
$SCRIPTS/installer/install_crypt.sh mac 7.0a
$SCRIPTS/installer/install_crypt.sh linux-$1 7.0a

# Deploy dot_files
$SCRIPTS/installer/install_dotfiles.sh

# Deploy dot_emacs
$SCRIPTS/installer/install_dotemacs.sh

# Termtter
$PRIVATE/installer/install_dottermtter.sh
$SCRIPTS/installer/install_termtter_plugins.sh

# Install plagger plugin
$SCRIPTS/installer/install_plagger_plugins.sh

# Mail to admin when startup
$SCRIPTS/installer/install_rclocal.sh

# rsyslog
if [ `aptitude search rsyslog | awk '/^i/' | wc -l` = 0 ]; then
    sudo aptitude -y install rsyslog
fi

# Remove Fonts
if [ `aptitude search xfonts-shinonome | awk '/^i/' | wc -l` != 0 ]; then
    sudo aptitude -y purge xfonts-shinonome
fi
if [ `aptitude search xfonts-mplus | awk '/^i/' | wc -l` != 0 ]; then
    sudo aptitude -y purge xfonts-mplus
fi

# Optional Libraries
if [ `aptitude search libxslt1-dev | awk '/^i/' | wc -l` = 0 ]; then
    sudo aptitude -y install libxslt1-dev libxml-dev
fi

# KVM
if [ `egrep '^flags.*(vmx|svm)' /proc/cpuinfo | wc -l` != 0 ]; then
    if [ `aptitude search libvirt-bin | awk '/^i/' | wc -l` = 0 ]; then
        sudo aptitude -y install kvm libvirt-bin
        sudo aptitude -y install python-libvirt
        #sudo aptitude -y install virt-manager
        sudo addgroup $USER libvirtd
        sudo addgroup $USER kvm
    fi
fi

# qemu
if [ `aptitude search kqemu-source | awk '/^i/' | wc -l` = 0 ]; then
    sudo aptitude -y install kqemu-source qemu
fi

# Auto Upgrade Job
sudo cp $SCRIPTS/cron/bin/auto-upgrade /etc/cron.daily/auto-upgrade
sudo vim /etc/cron.daily/auto-upgrade
sudo chmod 750 /etc/cron.daily/auto-upgrade
sudo chown root:adm /etc/cron.daily/auto-upgrade
# Daily Backup Job
sudo cp $SCRIPTS/cron/bin/backup.sh /root/bin/backup.sh
sudo vim /root/bin/backup.sh
sudo chmod 700 /root/bin/backup.sh
sudo cp $SCRIPTS/cron/bin/backup /etc/cron.daily/backup
sudo vim /etc/cron.daily/backup
sudo chmod 750 /etc/cron.daily/backup
sudo chown root:adm /etc/cron.daily/backup
# Server Resource Report Job
sudo cp $SCRIPTS/get_resources.sh /root/bin/get_resources.sh
sudo chmod 700 /root/bin/get_resources.sh
sudo chown -R root:root /root/bin/get_resources.sh
# ClamAV Auto Upgrade Job
sudo cp $SCRIPTS/cron/bin/clamav_upgrade /etc/cron.weekly/clamav_upgrade
sudo vim /etc/cron.weekly/clamav_upgrade
sudo chmod 750 /etc/cron.weekly/clamav_upgrade
sudo chown root:adm /etc/cron.weekly/clamav_upgrade

# Permissions for /src
sudo chown -R root:root /usr/src
sudo chown -R root:root /usr/local/src

# Last Setup
sudo vim /etc/hosts
sudo vim /etc/init.d/cron
# Disable motd
sudo vim /etc/pam.d/sshd
sudo vim /etc/pam.d/login

# Private settings
$PRIVATE/batch_configuration.sh

