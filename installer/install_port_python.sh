#!/bin/sh
#
########################################################################
# MacPorts python installer
#  $1 = version
#  $2 = uninstall
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 2014-04-12
#       Specify version and uninstall.
#  v1.1 2014-02-11
#       Fork from macports lang installer.
#  v1.0 2013-10-27
#       First.
########################################################################

install_ports() {
    sudo port -d $OPERATION python$VERSION
    sudo port -d $OPERATION py$VERSION-py
    sudo port -d $OPERATION py$VERSION-pip
    sudo port -d $OPERATION py$VERSION-setuptools
    sudo port -d $OPERATION py$VERSION-cython
    sudo port -d $OPERATION py$VERSION-ipython
    sudo port -d $OPERATION py$VERSION-docutils
    sudo port -d $OPERATION py$VERSION-nose
    sudo port -d $OPERATION py$VERSION-simplejson
    sudo port -d $OPERATION py$VERSION-msgpack
    sudo port -d $OPERATION py$VERSION-Pillow
    sudo port -d $OPERATION py$VERSION-numpy
    sudo port -d $OPERATION py$VERSION-scipy
    sudo port -d $OPERATION py$VERSION-scikit-learn
    sudo port -d $OPERATION py$VERSION-matplotlib
    sudo port -d $OPERATION py$VERSION-pandas
    sudo port -d $OPERATION py$VERSION-patsy
    sudo port -d $OPERATION py$VERSION-statsmodels
    sudo port -d $OPERATION py$VERSION-sympy
    sudo port -d $OPERATION py$VERSION-django
    sudo port -d $OPERATION py$VERSION-sqlalchemy
    sudo port -d $OPERATION py$VERSION-readline
    sudo port -d $OPERATION py$VERSION-babel
    sudo port -d $OPERATION py$VERSION-genshi
    sudo port -d $OPERATION py$VERSION-beautifulsoup4
    sudo port -d $OPERATION py$VERSION-lxml
    sudo port -d $OPERATION py$VERSION-requests
    sudo port -d $OPERATION py$VERSION-nltk3
    sudo port -d $OPERATION py$VERSION-jinja2
    sudo port -d $OPERATION py$VERSION-tornado
    sudo port -d $OPERATION py$VERSION-zmq
}

main() {
    test -n "$1" || VERSION=34
    test -n "$1" && VERSION=$1

    test -n "$2" || OPERATION=install
    test -n "$2" && OPERATION=$2
    install_ports $*
    port installed
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*
