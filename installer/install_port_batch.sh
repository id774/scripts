#!/bin/sh
#
########################################################################
# MacPorts batch installer
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 8/29,2008
#       Add option -d.
#  v1.0 8/15,2008
#       Stable.
########################################################################

sudo port -d selfupdate
sudo port -d install libiconv +enable_cp932fix
sudo port -d install coreutils +with_default_names
sudo port -d install findutils +with_default_names
sudo port -d install wget
sudo port -d install curl
sudo port -d install nkf
sudo port -d install screen
sudo port -d install ruby
sudo port -d install zlib
sudo port -d install openssl
sudo port -d install rb-rubygems
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
sudo port -d install mysql5
port installed

