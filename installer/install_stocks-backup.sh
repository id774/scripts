#!/bin/sh
#
########################################################################
# Install Stocks backup
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2017-05-29
#       First.
########################################################################

setup_environment() {
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
    sudo cp $OPTIONS $SCRIPTS/cron/etc/cron.d/stocks-backup /etc/cron.d/
    sudo chown root:adm /etc/cron.d/stocks-backup
    sudo chmod 640 /etc/cron.d/stocks-backup
}

install_script
