#!/bin/sh
#
########################################################################
# Install Python Libraries.
#  $1 = python path (ex. /usr/local)
#  $2 = no sudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2/9,2014
#       First.
########################################################################

setup_environment() {
    test -n "$1" && export EASY_INSTALL=$1/bin/easy_install
    test -n "$1" || export EASY_INSTALL=easy_install
    test -n "$1" && export PIP=$1/bin/pip
    test -n "$1" || export PIP=pip
    test -n "$2" || SUDO=sudo
    test -n "$2" && SUDO=
    test "$2" = "sudo" && SUDO=sudo
}

install_libs() {
    $SUDO $EASY_INSTALL IPython
    $SUDO $EASY_INSTALL nose
    $SUDO $EASY_INSTALL simplejson
    $SUDO $EASY_INSTALL -Z SQLAlchemy
    $SUDO $EASY_INSTALL Genshi
    $SUDO $EASY_INSTALL Babel
    $SUDO $EASY_INSTALL Pygments
    $SUDO $EASY_INSTALL web.py
    $SUDO $EASY_INSTALL python-twitter
    $SUDO $EASY_INSTALL -U bottle
    $SUDO $EASY_INSTALL -U cherrypy
    $SUDO $EASY_INSTALL -U distribute
    $SUDO $EASY_INSTALL pip
    $SUDO $PIP install -U numpy
    $SUDO $PIP install -U pyyaml nltk
}

main() {
    setup_environment $*
    install_libs $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*
