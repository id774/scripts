#!/bin/sh
#
########################################################################
# Install dot_vim
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 3/7,2010
#       Refactoring.
########################################################################

setup_environment() {
    TARGET=$HOME/.vim

    case $OSTYPE in
      *darwin*)
        OPTIONS=-Rv
        ;;
      *)
        OPTIONS=-Rvd
        ;;
    esac
}

install_dotvim() {
    setup_environment

    sudo cp $OPTIONS $SCRIPTS/cron/etc/cron.d/postgres-backup /etc/cron.d/
    test -d /var/lib/postgresql && test -d /var/lib/postgresql/backup || sudo mkdir -p /var/lib/postgresql/backup
    test -d /var/lib/postgresql/backup && sudo chown postgres:root /var/lib/postgresql/backup
    test -d /var/lib/postgresql/backup && sudo chmod 750 /var/lib/postgresql/backup
}

test -d $SCRIPTS/dot_files/dot_vim || exit 1
install_dotvim
