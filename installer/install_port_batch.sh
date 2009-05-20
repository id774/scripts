#!/bin/sh
#
########################################################################
# MacPorts batch installer
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.4 5/18,2009
#       Add PostgreSQL, launchd startup settings.
#  v1.3 1/22,2009
#       Add smartmontools.
#  v1.2 10/28,2008
#       Add MySQL server configuration.
#  v1.1 8/29,2008
#       Add option -d.
#  v1.0 8/15,2008
#       Stable.
########################################################################

sudo port -d selfupdate
sudo port -d install libiconv +enable_cp932fix
sudo port -d install coreutils
sudo port -d install findutils
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
sudo port -d install smartmontools
# MySQL
sudo port -d install mysql5 +server
sudo -u mysql mysql_install_db5
sudo cp /opt/local/share/mysql5/mysql/my-small.cnf /opt/local/etc/mysql5/my.cnf
sudo vim /opt/local/etc/mysql5/my.cnf
sudo launchctl load -w /Library/LaunchDaemons/org.macports.mysql5.plist
sudo /opt/local/share/mysql5/mysql/mysql.server start
mysql5 -u root -p
sudo port -d install rb-dbi +dbd_mysql
# PostgreSQL
#sudo port -d install postgresql83
#sudo port -d install postgresql83-server
#sudo launchctl load -w /Library/LaunchDaemons/org.macports.postgresql83-server.plist
#sudo mkdir -p /opt/local/var/db/postgresql83/defaultdb
#sudo chown postgres:postgres /opt/local/var/db/postgresql83/defaultdb
#sudo chown postgres:postgres /opt/local/var/db/postgresql83/
#sudo su postgres -c '/opt/local/lib/postgresql83/bin/initdb -D /opt/local/var/db/postgresql83/defaultdb'
#sudo su postgres -c '/opt/local/lib/postgresql83/bin/postgres -D /opt/local/var/db/postgresql83/defaultdb &'
#psql83 -d postgres -U postgres
port installed

