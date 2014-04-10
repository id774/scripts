#!/bin/sh
#
########################################################################
# MacPorts python installer
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 2/11,2014
#       Fork from macports lang installer.
#  v1.0 10/27,2013
#       First.
########################################################################

main() {
    VERSION=34

    sudo port -d install python$VERSION
    sudo port -d install py$VERSION-py
    sudo port -d install py$VERSION-pip
    sudo port -d install py$VERSION-setuptools
    sudo port -d install py$VERSION-cython
    sudo port -d install py$VERSION-ipython
    sudo port -d install py$VERSION-docutils
    sudo port -d install py$VERSION-nose
    sudo port -d install py$VERSION-simplejson
    sudo port -d install py$VERSION-msgpack
    sudo port -d install py$VERSION-Pillow
    sudo port -d install py$VERSION-numpy
    sudo port -d install py$VERSION-scipy
    sudo port -d install py$VERSION-scikit-learn
    sudo port -d install py$VERSION-matplotlib
    sudo port -d install py$VERSION-pandas
    sudo port -d install py$VERSION-patsy
    sudo port -d install py$VERSION-statsmodels
    sudo port -d install py$VERSION-sympy
    sudo port -d install py$VERSION-django
    sudo port -d install py$VERSION-sqlalchemy
    sudo port -d install py$VERSION-readline
    sudo port -d install py$VERSION-babel
    sudo port -d install py$VERSION-genshi
    sudo port -d install py$VERSION-beautifulsoup4
    sudo port -d install py$VERSION-lxml
    sudo port -d install py$VERSION-requests
    sudo port -d install py$VERSION-nltk3
    sudo port -d install py$VERSION-jinja2
    sudo port -d install py$VERSION-tornado
    sudo port -d install py$VERSION-zmq

    port installed
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main
