#!/bin/sh

export SCRIPTS=$HOME/scripts

chsh -s /bin/zsh
sudo chsh -s /bin/bash root

# APT Update
#DISTRIB_CODENAME=lenny
#SOURCESLIST=sources-$DISTRIB_CODENAME.list
#sudo cp $SCRIPTS/etc/$SOURCESLIST /etc/apt/sources.list
sudo vim /etc/apt/sources.list
sudo aptitude update

# Ubuntu-ja GPG Automatic Import (Ubuntu Server)
#eval `cat /etc/lsb-release`
#wget -q http://www.ubuntulinux.jp/ubuntu-ja-archive-keyring.gpg -O- | sudo apt-key add -
#sudo wget http://www.ubuntulinux.jp/sources.list.d/$DISTRIB_CODENAME.list -O /etc/apt/sources.list.d/ubuntu-ja.list
#sudo aptitude update

# Text Editor
sudo aptitude -y install emacs-snapshot emacs-snapshot-el
sudo aptitude -y purge emacs
sudo update-alternatives --config emacs
sudo aptitude -y install w3m-el-snapshot w3m-img imagemagick
sudo aptitude -y install vim-gui-common vim-runtime colordiff
sudo aptitude -y purge vim-gnome
sudo aptitude -y install ctags

# Bitstream Vera Sans Mono font(for Emacs)
sudo aptitude -y install ttf-vlgothic ttf-bitstream-vera

# Deploy dot_emacs
$SCRIPTS/installer/install_dotemacs.sh

# paco
$SCRIPTS/installer/install_paco.sh

# Ruby
$SCRIPTS/installer/install_ruby.sh 187-svn

# GDM Themes
#$SCRIPTS/installer/install_gdmthemes.sh
#$SCRIPTS/installer/install_gdmthemes2.sh

# Deploy dot_files
$SCRIPTS/installer/install_dotfiles.sh dot_xmodmaprc_hhklite2

# RubyGems
$SCRIPTS/installer/install_rubygems.sh
$SCRIPTS/installer/install_rails.sh 212
$SCRIPTS/installer/install_rails.sh 205
$SCRIPTS/installer/install_rails.sh 126

# Permissions for /src
sudo chown -R root:root /usr/src
sudo chown -R root:root /usr/local/src

# hddtemp
sudo aptitude -y install lm-sensors
sudo aptitude -y install hddtemp
sudo dpkg-reconfigure hddtemp

# sysstat
sudo aptitude -y install sysstat
sudo dpkg-reconfigure sysstat

# cron job
$SCRIPTS/installer/install_cronjob.sh

# fix python doc
$SCRIPTS/dev/fix_doc_python_empty_modindex.sh

# Last Setup
test -f ~/.viminfo && sudo chown $USER ~/.viminfo

# Upgrade

# Debian unstable
#sudo aptitude -v full-upgrade && sudo aptitude autoclean

# Debian stable/testing
sudo aptitude -y safe-upgrade && sudo aptitude autoclean

# Ubuntu Server
#sudo aptitude -y safe-upgrade && sudo aptitude -y install linux-image-server linux-server gnupg && sudo aptitude autoclean

# Ubuntu Desktop
#sudo aptitude -y safe-upgrade && sudo aptitude -y install linux-image-generic linux-generic linux-restricted-modules-generic linux-headers linux-headers-generic gnupg && sudo aptitude autoclean
