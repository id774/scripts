#!/bin/sh
#
########################################################################
# Install Python and pip
#  $1 = version
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2/9,2014
#       First.
########################################################################

install_python() {
    $SCRIPTS/installer/install_python.sh 3.3.3 /opt/python/3.3.3
    $SCRIPTS/installer/install_pip.sh /opt/python/3.3.3
}

operation() {
    test -n "$1" || VERSION=3.3.3
    test -n "$1" && VERSION=$1
    test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
    test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts
    install_python $*
}

operation $*
