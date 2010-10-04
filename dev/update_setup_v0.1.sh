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
~/local/github/dot_emacs/install_dotemacs.sh

# paco
$SCRIPTS/installer/install_paco.sh

# Vim (Original Build)
#$SCRIPTS/installer/install_ncurses.sh
#$SCRIPTS/installer/install_vim.sh

# Ruby
sudo aptitude -y install ruby1.8 ruby1.8-dev rubygems
$SCRIPTS/installer/install_ruby.sh 187-svn

# Django
$SCRIPTS/installer/install_django.sh 1.0.2

# GDM Themes
#$SCRIPTS/installer/install_gdmthemes.sh
#$SCRIPTS/installer/install_gdmthemes2.sh

# Deploy dot_files
$SCRIPTS/installer/install_dotfiles.sh dot_xmodmaprc_hhklite2

# RubyGems
$SCRIPTS/installer/install_rubygems.sh
sudo gem uninstall rails
sudo gem uninstall actionmailer
sudo gem uninstall actionpack
sudo gem uninstall activerecord
sudo gem uninstall activeresource
sudo gem uninstall activesupport
$SCRIPTS/installer/install_rails.sh 212
$SCRIPTS/installer/install_rails.sh 205
$SCRIPTS/installer/install_rails.sh 126
sudo gem install rspec
sudo gem install rspec-rails
sudo gem install cucumber

# Crypt
$SCRIPTS/installer/install_crypt.sh 6.1a-ubuntu-x86

# Permissions for /src
sudo chown -R root:root /usr/src
sudo chown -R root:root /usr/local/src

# Monitoring Tools
# sysstat
sudo aptitude -y install sysstat
sudo dpkg-reconfigure sysstat
# ENABLED="true"
sudo vim /etc/default/sysstat
# hddtemp
sudo aptitude -y install lm-sensors
sudo aptitude -y install hddtemp
sudo dpkg-reconfigure hddtemp
# smartmontools
sudo aptitude -y install smartmontools
# start_smartd=yes
# smartd_opts="--interval=7200"
sudo vim /etc/default/smartmontools
# smartmontools
sudo aptitude -y install ext2resize

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
