#!/bin/sh
#
########################################################################
# Install PostgreSQL backup
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 2016-07-29
#       Change dirname, error check.
#  v0.1 2016-07-27
#       First.
########################################################################

setup_environment() {
    test -d /var/lib/postgresql || test -d /var/lib/pgsql || exit 1
    test -d /var/lib/postgresql && TARGET=/var/lib/postgresql
    test -d /var/lib/pgsql && TARGET=/var/lib/pgsql

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
    test -d $TARGET && test -d $TARGET/pg_dump || sudo mkdir -v $TARGET/pg_dump
    test -d $TARGET/pg_dump && sudo chown postgres:postgres $TARGET/pg_dump
    test -d $TARGET/pg_dump && sudo chmod 750 $TARGET/pg_dump
}

install_script
