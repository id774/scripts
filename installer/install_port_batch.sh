#!/bin/sh
#
########################################################################
# MacPorts batch installer
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

sudo port selfupdate
sudo port install libiconv +enable_cp932fix
sudo port install coreutils +with_default_names
sudo port install findutils +with_default_names
sudo port install wget
sudo port install nkf
sudo port install screen
sudo port install ruby
sudo port install rb-rubygems
sudo port install subversion
sudo port install -f svk
sudo port install git-core
sudo port install lv
sudo port install chasen
sudo port install sqlite3
sudo port install libxml
sudo port install libxml2
port installed

