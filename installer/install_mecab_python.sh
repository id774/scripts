#!/bin/sh
#
########################################################################
# Install Mecab Python Binding
#  $1 = python path
#  $2 = source path
#  $3 = no sudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2/9,2014
#       First.
########################################################################

setup_environment() {
    test -n "$1" && export PYTHON=$1/bin/python
    test -n "$1" || export PYTHON=/usr/local/bin/python
    test -n "$2" && export TARGET=$2
    test -n "$2" || export TARGET=/usr/local/src/mecab/mecab-python-0.994
    test -n "$3" || SUDO=sudo
    test -n "$3" && SUDO=
    test "$3" = "sudo" && SUDO=sudo
}

make_and_install() {
    cd $TARGET
    $SUDO $PYTHON setup.py build
    $SUDO $PYTHON setup.py install
}

install_mecab() {
    setup_environment $*
    make_and_install $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_mecab $*
