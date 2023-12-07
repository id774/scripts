#!/bin/bash
#
########################################################################
# Install System Administration Scripts
#  $1 = uninstall
#  $2 = install path (ex. /usr/local/sbin)
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.8 2023-11-14
#       Add veramount.
#  v0.7 2023-06-23
#       Fix uninstall bug, add md5, remove some obsolete files.
#  v0.6 2014-05-24
#       Using bash, switch arguments, show uninstall messages.
#  v0.5 2011-06-28
#       Uninstall enabled, RHEL scripts.
#  v0.4 2011-05-27
#       Debian only scripts was moved.
#  v0.3 2011-03-25
#       Install dpkg-hold for Debian.
#  v0.2 2010-12-04
#       Fix copy option on OS X.
#  v0.1 2010-11-16
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
        md5 \
        chmodtree \
        cltmp \
        copydir \
        namecalc \
        now \
        waitlock \
        swapext \
        git-follow-origin \
        git-co-remote-branch \
        git-ignore \
        pyck \
        autopyck \
        get_resources \
        dpkg-hold \
        gpg-import \
        tcmount \
        veramount \
        platex2pdf \
        userlist \
        usershells \
        port-upgrade \
        port-cleanup
}

install_scripts() {
    sudo cp -v $OPTIONS $SCRIPTS/$2 $SBIN/$3
    sudo chmod $1 $SBIN/$3
    sudo chown $OWNER $SBIN/$3
}

setup_scripts() {
    install_scripts 755 md5.py md5
    install_scripts 755 chmodtree.py chmodtree
    install_scripts 755 cltmp.sh cltmp
    install_scripts 755 userlist.sh userlist
    install_scripts 755 usershells.py usershells
    install_scripts 755 git-follow-origin.sh git-follow-origin
    install_scripts 755 git-co-remote-branch.sh git-co-remote-branch
}

setup_darwin_scripts() {
    install_scripts 755 port-upgrade.sh port-upgrade
    install_scripts 755 port-cleanup.sh port-cleanup
}

setup_debian_scripts() {
    install_scripts 755 dpkg-hold.sh dpkg-hold
    install_scripts 755 gpg-import.sh gpg-import
    install_scripts 755 veramount.sh veramount
}

install_sysadmin_scripts() {
    setup_scripts
    test -f /etc/debian_version && setup_debian_scripts
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
