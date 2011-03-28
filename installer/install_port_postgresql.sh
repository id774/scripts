#!/bin/sh
#
########################################################################
# MacPorts postgresql installer
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 3/28,2011
#       Split.
########################################################################

main() {
    sudo port -d selfupdate
    sudo port -d install postgresql83
    sudo port -d install postgresql83-server
    sudo launchctl load -w /Library/LaunchDaemons/org.macports.postgresql83-server.plist
    sudo mkdir -p /opt/local/var/db/postgresql83/defaultdb
    sudo chown postgres:postgres /opt/local/var/db/postgresql83/defaultdb
    sudo chown postgres:postgres /opt/local/var/db/postgresql83/
    sudo su postgres -c '/opt/local/lib/postgresql83/bin/initdb -D /opt/local/var/db/postgresql83/defaultdb'
    sudo su postgres -c '/opt/local/lib/postgresql83/bin/postgres -D /opt/local/var/db/postgresql83/defaultdb &'
    psql83 -d postgres -U postgres
    port installed
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
main
