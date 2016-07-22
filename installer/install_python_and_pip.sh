#!/bin/sh
#
########################################################################
# Install Python and pip
#  $1 = version
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 2/14,2014
#       Improvement versioning.
#  v0.1 2/9,2014
#       First.
########################################################################

install_python() {
    $SCRIPTS/installer/install_python.sh $VERSION /opt/python/$PATH_VERSION
    $SCRIPTS/installer/install_pip.sh /opt/python/$PATH_VERSION
}

operation() {
    test -n "$1" || VERSION=3.5.2
    test -n "$1" && VERSION=$1
    test -n "$2" || PATH_VERSION=3.5
    test -n "$2" && PATH_VERSION=$1
    test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
    test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts
    install_python $*
}

operation $*
