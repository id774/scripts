#!/bin/sh
#
########################################################################
# Install CaboCha Python Binding
#  $1 = python path
#  $2 = source path
#  $3 = no sudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2/10,2014
#       First.
########################################################################

setup_environment() {
    test -n "$1" && export PYTHON=$1/bin/python
    test -n "$1" || export PYTHON=/usr/local/bin/python
    test -n "$2" && export TARGET=$2
    test -n "$2" || export TARGET=/usr/local/src/cabocha/cabocha-0.67/python
    test -n "$3" || SUDO=sudo
    test -n "$3" && SUDO=
    test "$3" = "sudo" && SUDO=sudo
}

source_compile() {
    cd $TARGET
    $SUDO $PYTHON setup.py build_ext
    $SUDO $PYTHON setup.py install
}

install_mecab() {
    setup_environment $*
    source_compile $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_mecab $*
