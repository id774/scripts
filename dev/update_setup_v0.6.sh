#!/bin/sh
#
# This scripts updates environment from 0.6 to 0.7
########################################################################

export SCRIPTS=$HOME/scripts
export PRIVATE=$HOME/private/scripts

# APT Update
DISTRIB_CODENAME=squeeze
test -f /etc/lsb-release && DISTRIB_CODENAME=lucid
SOURCESLIST=sources-$DISTRIB_CODENAME.list
sudo cp $SCRIPTS/etc/$SOURCESLIST /etc/apt/sources.list
sudo vim /etc/apt/sources.list
sudo apt-get update

# iptables
$SCRIPTS/installer/install_iptables.sh

# Deploy dot_files
~/local/github/dot_emacs/install_dotemacs.sh
~/local/github/dot_zsh/install_dotzsh.sh
$SCRIPTS/installer/install_dotfiles.sh
$SCRIPTS/installer/install_dotvim.sh

# Termtter
$PRIVATE/installer/install_dottermtter.sh
$SCRIPTS/installer/install_termtter_plugins.sh

# cronjob
$SCRIPTS/installer/install_cronjob.sh

# sysadmin scripts
$SCRIPTS/installer/setup_sysadmin_scripts.sh

# Crypt
$SCRIPTS/installer/install_crypt.sh src 7.1
$SCRIPTS/installer/install_crypt.sh win 7.1
$SCRIPTS/installer/install_crypt.sh mac 7.1
$SCRIPTS/installer/install_crypt.sh linux-$1 7.1

sudo rm -f /etc/zsh/plugins/incr.zsh*
