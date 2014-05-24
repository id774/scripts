#!/bin/bash
#
########################################################################
# Install System Administration Scripts
#  $1 = uninstall
#  $2 = install path (ex. /usr/local/sbin)
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.6 5/24,2014
#       Using bash, switch arguments, show uninstall messages.
#  v0.5 6/28,2011
#       Uninstall enabled, RHEL scripts.
#  v0.4 5/27,2011
#       Debian only scripts was moved.
#  v0.3 3/25,2011
#       Install dpkg-hold for Debian.
#  v0.2 12/04,2010
#       Fix copy option on OS X.
#  v0.1 11/16,2010
#       First.
########################################################################

setup_environment() {
    test -n "$2" && SBIN=$2
    test -n "$2" || SBIN=/usr/local/sbin
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

uninstall_scripts() {
    while [ $# -gt 0 ]
    do
        test -f $SBIN/$1 && echo -n "Removing: " && sudo rm -vf $SBIN/$1
        shift
    done
}

uninstall_sysadmin_scripts() {
    uninstall_scripts \
        chmodtree \
        cltmp \
        copydir \
        namecalc \
        waitlock \
        swapext \
        git-follow-origin \
        get_resources \
        tcmount \
        dpkg-hold \
        platex2pdf \
        port-upgrade
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
    install_scripts 755 namecalc.rb namecalc
    install_scripts 755 waitlock.rb waitlock
    install_scripts 755 swapext.py swapext
    install_scripts 755 git-follow-origin.sh git-follow-origin
}

setup_darwin_scripts() {
    install_scripts 755 port-upgrade.sh port-upgrade
    install_scripts 755 port-cleanup.sh port-cleanup
}

setup_rhel_scripts() {
    install_scripts 755 get_resources.sh get_resources
    install_scripts 755 tcmount.py tcmount
}

setup_debian_scripts() {
    install_scripts 755 dpkg-hold.sh dpkg-hold
    install_scripts 755 get_resources.sh get_resources
    install_scripts 755 tcmount.py tcmount
    install_scripts 755 platex2pdf.sh platex2pdf
}

install_sysadmin_scripts() {
    setup_scripts
    test -f /etc/debian_version && setup_debian_scripts
    test -f /etc/redhat-release && setup_rhel_scripts
    case $OSTYPE in
      *darwin*)
        setup_darwin_scripts
        ;;
    esac
}

setup_sysadmin_scripts() {
    setup_environment $*
    test -n "$1" && uninstall_sysadmin_scripts
    test -n "$1" || install_sysadmin_scripts
    test "$1" = "install" && install_sysadmin_scripts
}

test -n "$SCRIPTS" || exit 1
setup_sysadmin_scripts $*
