#!/bin/sh
#
########################################################################
# Install Python and pip
#  $1 = version
#  $2 = dir name
#  ex. $SCRIPTS/installer/install_python_and_pip.sh 3.12.1 3.12
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 2023-12-08
#       Error if there is no argument.
#  v0.2 2014-02-14
#       Improvement versioning.
#  v0.1 2014-02-09
#       First.
########################################################################

usage() {
    echo Not enough arguments.
    echo ex. $SCRIPTS/installer/install_python_and_pip.sh 3.12.1 3.12
    exit 1
}

install_python() {
    $SCRIPTS/installer/install_python.sh $VERSION /opt/python/$PATH_VERSION
    $SCRIPTS/installer/install_pip.sh /opt/python/$PATH_VERSION
}

operation() {
    test -n "$1" || usage
    test -n "$1" && VERSION=$1
    test -n "$2" || usage
    test -n "$2" && PATH_VERSION=$2
    test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
    test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts
    install_python $*
}

operation $*
