#!/bin/sh
#
# This scripts updates environment from 0.6 to 0.7
########################################################################

export SCRIPTS=$HOME/scripts
export PRIVATE=$HOME/private/scripts

# Need i386 or amd64 option
test -n "$1" || exit 1

# APT Update
DISTRIB_CODENAME=squeeze
test -f /etc/lsb-release && DISTRIB_CODENAME=lucid
SOURCESLIST=sources-$DISTRIB_CODENAME.list
sudo cp $SCRIPTS/etc/$SOURCESLIST /etc/apt/sources.list
sudo vim /etc/apt/sources.list
sudo apt-get update

# Crypt
$SCRIPTS/installer/install_des.sh
$SCRIPTS/installer/install_crypt.sh src
$SCRIPTS/installer/install_crypt.sh win
$SCRIPTS/installer/install_crypt.sh mac
$SCRIPTS/installer/install_crypt.sh linux-$1

# iptables
$SCRIPTS/installer/install_iptables.sh

# Deploy dot_files
~/local/github/dot_emacs/install_dotemacs.sh
~/local/github/dot_zsh/install_dotzsh.sh
$SCRIPTS/installer/install_dotfiles.sh
$SCRIPTS/installer/install_dotvim.sh

# CoffeeScript
$SCRIPTS/installer/install_coffeescript.sh

# Termtter
$PRIVATE/installer/install_dottermtter.sh
$SCRIPTS/installer/install_termtter_plugins.sh

# cronjob
$SCRIPTS/installer/install_cronjob.sh

# sysadmin scripts
$SCRIPTS/installer/setup_sysadmin_scripts.sh

sudo rm -f /etc/zsh/plugins/incr.zsh*
