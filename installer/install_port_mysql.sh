#!/bin/sh
#
########################################################################
# MacPorts mysql installer
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2011-03-28
#       Split.
########################################################################

main() {
    sudo port -d selfupdate
    sudo port -d install mysql5 +server
    sudo -u mysql mysql_install_db5
    sudo cp /opt/local/share/mysql5/mysql/my-small.cnf /opt/local/etc/mysql5/my.cnf
    sudo vi /opt/local/etc/mysql5/my.cnf
    sudo launchctl load -w /Library/LaunchDaemons/org.macports.mysql5.plist
    sudo /opt/local/share/mysql5/mysql/mysql.server start
    mysql5 -u root -p
    sudo port -d install rb-dbi +dbd_mysql
    port installed
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main
