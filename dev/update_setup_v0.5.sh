#!/bin/sh
#
# This scripts updates environment from 0.5 to 0.6
# $1 = i386, or amd64
########################################################################

export SCRIPTS=$HOME/scripts
export PRIVATE=$HOME/private/scripts

# iptables
$SCRIPTS/installer/install_iptables.sh

# RubyGems
$SCRIPTS/installer/install_rubygems.sh 162 /opt/ruby/1.9.2
$SCRIPTS/installer/install_gems.sh /opt/ruby/1.9.2
$SCRIPTS/installer/install_rails.sh 300 /opt/ruby/1.9.2
/opt/ruby/1.9.2/bin/vim-ruby-install.rb

# Passenger
$SCRIPTS/installer/install_passenger.sh /opt/ruby/1.9.2
$SCRIPTS/config/update-alternatives-ruby.sh

# Java JDK
sudo apt-get -y install openjdk-6-jdk

# Mew
sudo apt-get -y install mew stunnel ca-certificates

# Deploy dot_files
~/local/github/dot_emacs/install_dotemacs.sh
~/local/github/dot_zsh/install_dotzsh.sh
$SCRIPTS/installer/install_dotfiles.sh
$SCRIPTS/installer/install_dotvim.sh

# Termtter
$PRIVATE/installer/install_dottermtter.sh
$SCRIPTS/installer/install_termtter_plugins.sh

# Permissions for /src
sudo chown -R root:root /usr/src
sudo chown -R root:root /usr/local/src

# sysadmin scripts
$SCRIPTS/installer/setup_sysadmin_scripts.sh

