#!/bin/sh
#
########################################################################
# Install Go (for linux-amd64)
#  $1 = version
#  $2 = install target
#  $3 = no sudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 2019-10-18
#       Stable.
########################################################################

setup_environment() {
    test -n "$2" || TARGET=/usr/local/go
    test -n "$2" && TARGET=$2
    test -n "$3" || SUDO=sudo
    test -n "$3" && SUDO=
    test "$3" = "sudo" && SUDO=sudo
    OPTIONS=-a
    OWNER=root:root
}

get_golang() {
    mkdir install_go
    cd install_go
    curl -L https://dl.google.com/go/go$1.linux-amd64.tar.gz -O
    test -f go$1.linux-amd64.tar.gz || exit 1
    tar xzvf go$1.linux-amd64.tar.gz
    mv go $1
    test -d $TARGET || $SUDO mkdir -p $TARGET
    $SUDO mv $1 $TARGET/
    cd ..
    $SUDO rm -rf install_go
}

install_go() {
    setup_environment $*
    test -n "$1" || exit 1
    get_golang $*
    go version
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_go $*
