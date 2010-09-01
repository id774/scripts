#!/bin/sh
#
# This scripts updates environment from 0.4 to 0.5
# $1 = i386, or amd64
########################################################################

export SCRIPTS=$HOME/scripts
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
vim-ruby-install.rb

# Passenger
$SCRIPTS/installer/install_passenger.sh /opt/ruby/1.9.2
$SCRIPTS/config/update-alternatives-ruby.sh

# Web crawler and HTML/XML parser
$SCRIPTS/installer/install_beautifulsoup.sh

# Crypt
$SCRIPTS/installer/install_crypt.sh src
$SCRIPTS/installer/install_crypt.sh win
$SCRIPTS/installer/install_crypt.sh mac
$SCRIPTS/installer/install_crypt.sh linux-$1

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

# Optional Libraries
if [ `aptitude search libxslt1-dev | awk '/^i/' | wc -l` = 0 ]; then
    sudo aptitude -y install libxslt1-dev libxml-dev
fi

# KVM
if [ `aptitude search libvirt-bin | awk '/^i/' | wc -l` = 0 ]; then
    sudo aptitude -y install kvm libvirt-bin
    sudo aptitude -y install python-libvirt
    sudo addgroup libvirtd $USER
fi

# qemu
if [ `aptitude search kqemu-source | awk '/^i/' | wc -l` = 0 ]; then
    sudo aptitude -y install kqemu-source qemu
fi

# Permissions for /src
sudo chown -R root:root /usr/src
sudo chown -R root:root /usr/local/src

# Last Setup
sudo vim /etc/hosts
sudo vim /etc/init.d/cron
# Disable motd
sudo vim /etc/pam.d/sshd
sudo vim /etc/pam.d/login

