#!/bin/sh
#
########################################################################
# Install System Administration Scripts
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 3/25,2011
#       Install dpkg-hold for Debian.
#  v0.2 12/04,2010
#       Fix copy option on OS X.
#  v0.1 11/16,2010
#       First.
########################################################################

setup_environment() {
    test -n "$1" && export SBIN=$1
    test -n "$1" || export SBIN=/usr/local/sbin
    case $OSTYPE in
      *darwin*)
        OPTIONS=-pR
        OWNER=root:wheel
        ;;
      *)
        OPTIONS=-a
        OWNER=root:root
        ;;
    esac
}

install_scripts() {
    sudo cp -v $OPTIONS $SCRIPTS/$2 $SBIN/$3
    sudo chmod $1 $SBIN/$3
    sudo chown $OWNER $SBIN/$3
}

setup_scripts() {
    install_scripts 755 chmodtree.py chmodtree
    install_scripts 755 cltmp.sh cltmp
    install_scripts 755 copydir.py copydir
    install_scripts 755 get_resources.sh get_resources
    install_scripts 755 tcmount.py tcmount
    install_scripts 755 namecalc.rb namecalc
    install_scripts 755 waitlock.rb waitlock
    install_scripts 755 swapext.py swapext
}

setup_debian_scripts() {
    install_scripts 755 dpkg-hold.sh dpkg-hold
}

setup_sysadmin_scripts() {
    setup_environment $*
    setup_scripts
    if [ -f /etc/debian_version ]; then
        setup_debian_scripts
    fi
}

test -n "$SCRIPTS" || exit 1
setup_sysadmin_scripts $*
