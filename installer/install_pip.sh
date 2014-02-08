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
    $SUDO $PIP install -U pip
    $SUDO $PIP install IPython
    $SUDO $PIP install nose
    $SUDO $PIP install simplejson
    $SUDO $PIP install numpy
    $SUDO $PIP install scipy
    $SUDO $PIP install django
    $SUDO $PIP install SQLAlchemy
    $SUDO $PIP install Genshi
    $SUDO $PIP install Babel
    $SUDO $PIP install Pygments
    $SUDO $PIP install web.py
    $SUDO $PIP install bottle
    $SUDO $PIP install cherrypy
}

main() {
    setup_environment $*
    install_libs $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*
