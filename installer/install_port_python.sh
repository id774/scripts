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
    sudo port -d install python33
    sudo port -d install py33-py
    sudo port -d install py33-pip
    sudo port -d install py33-setuptools
    sudo port -d install py33-ipython
    sudo port -d install py33-docutils
    sudo port -d install py33-nose
    sudo port -d install py33-simplejson
    sudo port -d install py33-msgpack
    sudo port -d install py33-Pillow
    sudo port -d install py33-cython
    sudo port -d install py33-numpy
    sudo port -d install py33-scipy
    sudo port -d install py33-scikit-learn
    sudo port -d install py33-matplotlib
    sudo port -d install py33-pandas
    sudo port -d install py33-patsy
    sudo port -d install py33-statsmodels
    sudo port -d install py33-django
    sudo port -d install py33-sqlalchemy
    sudo port -d install py33-readline
    sudo port -d install py33-babel
    sudo port -d install py33-genshi
    sudo port -d install py33-beautifulsoup4
    sudo port -d install py33-requests
    sudo port -d install py33-nltk3
    port installed
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main
