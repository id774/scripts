#!/bin/sh
#
########################################################################
# Install netbeans
#
#  $1 = version
#  $2 = revision
#  $3 = path
#  $4 = no sudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 6/29,2014
#       First.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=8.0
    test -n "$1" && VERSION=$1
    test -n "$2" || REVISION=201403101706
    test -n "$2" && REVISION=$2
    test -n "$3" || TARGET=/opt/netbeans
    test -n "$3" && TARGET=$3
    test -n "$4" || SUDO=sudo
    test -n "$4" && SUDO=
    test "$4" = "sudo" && SUDO=sudo

    case $OSTYPE in
      *darwin*)
        OPTIONS=-Rv
        OWNER=root:wheel
        ;;
      *)
        OPTIONS=-Rvd
        OWNER=root:root
        ;;
    esac
}

install_netbeans() {
    setup_environment $*
    mkdir install_netbeans
    cd install_netbeans
    wget "http://id774.net/java/netbeans-$VERSION-$REVISION.zip"
    unzip netbeans-$VERSION-$REVISION.zip
    mv netbeans $VERSION
    test -d $TARGET || $SUDO mkdir $TARGET
    $SUDO cp $OPTIONS $VERSION $TARGET
    cd ..
    rm -rf install_netbeans
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_netbeans $*
