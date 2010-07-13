#!/bin/sh
#
# This scripts updates environment of Mac OS X from 0.3 to 0.4
########################################################################

export SCRIPTS=$HOME/scripts

# Deploy dot_emacs
$SCRIPTS/installer/install_dotemacs.sh /usr/bin/emacs

# Deploy dot_files
$SCRIPTS/installer/install_dotfiles.sh
$SCRIPTS/installer/install_dotssh_mac.sh

# Web Application Framework
$SCRIPTS/installer/install_django.sh 1.1.1

# MacPorts Update
sudo port -d selfupdate
sudo port -d sync

# Termtter
sudo gem install termtter

# Install new package
sudo port -d install w3m
sudo port -d install emacs-w3m
sudo port -d install wget
sudo port -d install ruby
#$SCRIPTS/installer/install_port_python31.sh install
#$SCRIPTS/installer/install_port_ruby19.sh install

# Ruby
#$SCRIPTS/installer/install_ruby.sh 191-429 /opt/ruby/1.9.1

# RubyGems
$SCRIPTS/installer/install_rubygems.sh 137 /opt/local
$SCRIPTS/installer/install_gems.sh /opt/local
$SCRIPTS/installer/install_rails.sh rails-ruby19 /opt/local
#$SCRIPTS/installer/install_rails.sh 212
#$SCRIPTS/installer/install_rails.sh 205
#$SCRIPTS/installer/install_rails.sh 126
vim-ruby-install.rb

# Upgrade all package
sudo port -d -u upgrade outdated

# Install leak check
sudo port -d install libiconv +enable_cp932fix
sudo port -d install coreutils
sudo port -d install findutils
sudo port -d install wget
sudo port -d install curl
sudo port -d install nkf
sudo port -d install screen
sudo port -d uninstall ruby
sudo port -d install zlib
sudo port -d install openssl
sudo port -d uninstall rb-rubygems
sudo port -d install subversion
sudo port -d install git-core
sudo port -d install -f svk
sudo port -d install lv
sudo port -d install chasen
sudo port -d install sqlite3
sudo port -d install libxml
sudo port -d install libxml2
sudo port -d install expat
sudo port -d install p7zip
sudo port -d install bzip2
sudo port -d install ctags
sudo port -d install ncurses
sudo port -d install vim
sudo port -d install smartmontools
sudo port -d install w3m
sudo port -d install emacs-w3m

sudo port -d install python25
sudo port -d install py25-hashlib
sudo port -d install py25-zlib
sudo port -d install py25-setuptools
sudo port -d install py25-readline
sudo port -d install py25-mysql
sudo port -d install py25-twisted
sudo port -d install py25-pgsql
sudo port -d install py25-mechanize
sudo port -d install py25-openssl
sudo port -d install py25-paste
sudo port -d install py25-pastedeploy
sudo port -d install py25-simplejson
sudo port -d install py25-nose
sudo port -d install py25-sqlalchemy
sudo port -d install py25-sqlalchemy-migrate
sudo port -d install py25-turbogears
sudo port -d install py25-memcached
sudo port -d install py25-yaml

