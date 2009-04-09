#!/bin/sh
#
# This scripts updates environment from 0.2 to 0.3
########################################################################

export SCRIPTS=$HOME/scripts

# Deploy dot_emacs
$SCRIPTS/installer/install_dotemacs.sh

# Uninstall local build vim
#$SCRIPTS/installer/uninstall_vim.sh

# Deploy dot_files
$SCRIPTS/installer/install_dotfiles.sh dot_xmodmaprc_hhklite2

# Last Setup
sudo cp $SCRIPTS/etc/sudoers /etc/sudoers
sudo vim /etc/sudoers

# Upgrade

# Debian unstable
#sudo aptitude -v full-upgrade && sudo aptitude autoclean

# Debian stable/testing
sudo aptitude -y safe-upgrade && sudo aptitude autoclean

# Ubuntu Server
#sudo aptitude -y safe-upgrade && sudo aptitude -y install linux-image-server linux-server gnupg && sudo aptitude autoclean

# Ubuntu Desktop
#sudo aptitude -y safe-upgrade && sudo aptitude -y install linux-image-generic linux-generic linux-restricted-modules-generic linux-headers linux-headers-generic gnupg && sudo aptitude autoclean
