#!/bin/sh
#
# This scripts updates environment from 0.5 to 0.6
# $1 = i386, or amd64
########################################################################

export SCRIPTS=$HOME/scripts
export PRIVATE=$HOME/private/scripts

# Need i386 or amd64 option
test -n "$1" || exit 1

# APT Update
DISTRIB_CODENAME=lucid
SOURCESLIST=sources-$DISTRIB_CODENAME.list
sudo cp $SCRIPTS/etc/$SOURCESLIST /etc/apt/sources.list
sudo vim /etc/apt/sources.list
sudo aptitude update

sudo chsh -s /bin/sh root
$SCRIPTS/installer/install_mysql.sh
sudo apt-get install -y libcurl4-gnutls-dev
$SCRIPTS/installer/install_dottoprc.sh

# Java JDK
sudo apt-get -y install openjdk-6-jdk

# Mew
sudo apt-get -y install mew

# Deploy dot_files
$SCRIPTS/installer/install_dotfiles.sh
~/local/github/dot_emacs/install_dotemacs.sh

# Termtter
$PRIVATE/installer/install_dottermtter.sh
$SCRIPTS/installer/install_termtter_plugins.sh

# cronjob
$SCRIPTS/installer/install_cronjob.sh

# Permissions for /src
sudo chown -R root:root /usr/src
sudo chown -R root:root /usr/local/src

# sysadmin scripts
$SCRIPTS/installer/setup_sysadmin_scripts.sh

# GnuPG
test -d $HOME/.gnupg || $PRIVATE/installer/install_dotgnupg.sh

# hosts
cp ~/private/secure/dot_ssh/known_hosts ~/.ssh/known_hosts
sudo vim /etc/hosts $PRIVATE/etc/hosts

