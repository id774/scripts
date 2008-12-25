#!/bin/sh

export SCRIPTS=$HOME/scripts

chsh -s /bin/zsh
sudo chsh -s /bin/bash root

# Text Editor
sudo aptitude -y install emacs
sudo aptitude -y install emacs-snapshot emacs-snapshot-el
sudo update-alternatives --config emacs
sudo aptitude -y install vim-gui-common vim-runtime colordiff
sudo aptitude -y purge vim-gnome
sudo aptitude -y install ctags

# Bitstream Vera Sans Mono font(for Emacs)
sudo aptitude -y install ttf-vlgothic ttf-bitstream-vera

# Deploy dot_emacs
$SCRIPTS/installer/install_dotemacs.sh

# Ruby
$SCRIPTS/installer/install_ruby.sh 187-svn

# GDM Themes
#$SCRIPTS/installer/install_gdmthemes.sh
#$SCRIPTS/installer/install_gdmthemes2.sh

# Deploy dot_files
$SCRIPTS/installer/install_dotfiles.sh dot_xmodmaprc_hhklite2

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

test -f ~/.viminfo && sudo chown $USER ~/.viminfo

