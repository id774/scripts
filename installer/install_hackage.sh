#!/bin/sh
#
########################################################################
# Install hackages.
#  $1 = no sudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2/6,2014
#       First.
########################################################################

install_hackage() {
    test -n "$1" || SUDO=sudo
    test -n "$1" && SUDO=
    test "$1" = "sudo" && SUDO=sudo
    $SUDO cabal update
    $SUDO cabal install CPL
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_hackage $*
