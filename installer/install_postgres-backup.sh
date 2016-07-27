#!/bin/sh
#
########################################################################
# Install PostgreSQL backup
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 7/27,2016
#       First.
########################################################################

setup_environment() {
    TARGET=/var/lib/postgresql

    case $OSTYPE in
      *darwin*)
        OPTIONS=-Rv
        ;;
      *)
        OPTIONS=-Rvd
        ;;
    esac
}

install_script() {
    setup_environment

    sudo cp $OPTIONS $SCRIPTS/cron/etc/cron.d/postgres-backup /etc/cron.d/
    test -d $TARGET && test -d $TARGET/backup || sudo mkdir -p $TARGET/backup
    test -d $TARGET/backup && sudo chown postgres:postgres $TARGET/backup
    test -d $TARGET/backup && sudo chmod 750 $TARGET/backup
}

test -d $TARGET || exit 1
install_script
