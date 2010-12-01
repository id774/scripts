#!/bin/sh
#
########################################################################
# Install System Administration Scripts
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 11/16,2010
#       First.
########################################################################

setup_environment() {
    test -n "$1" && export SBIN=$1
    test -n "$1" || export SBIN=/usr/local/sbin
    case $OSTYPE in
      *darwin*)
        OWNER=root:wheel
        ;;
      *)
        OWNER=root:root
        ;;
    esac
}

install_scripts() {
    sudo cp -av $SCRIPTS/$2 $SBIN/$3
    sudo chmod $1 $SBIN/$3
    sudo chown $OWNER $SBIN/$3
}

setup_sysadmin_scripts() {
    setup_environment $*
    install_scripts 755 chmodtree.py chmodtree
    install_scripts 755 cltmp.sh cltmp
    install_scripts 755 copydir.py copydir
    install_scripts 755 get_resources.sh get_resources
    install_scripts 755 tcmount.py tcmount
    install_scripts 755 raphro.rb raphro
    install_scripts 755 waitlock.rb waitlock
}

test -n "$SCRIPTS" || exit 1
setup_sysadmin_scripts $*
