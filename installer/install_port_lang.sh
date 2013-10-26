#!/bin/sh
#
########################################################################
# MacPorts lang installer
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 10/27,2013
#       First.
########################################################################

main() {
    sudo port -d install ruby
    sudo port -d install nodejs
    sudo port -d install nodejs-devel
    sudo port -d install npm
    sudo port -d install coffee-script
    sudo port -d install nasm
    sudo port -d install gauche
    sudo port -d install clisp
    sudo port -d install scheme-48
    sudo port -d install gst
    sudo port -d install R
    sudo port -d install ghc
    sudo port -d install global
    sudo port -d install graphviz
    sudo port -d install graphviz-devel
    sudo port -d install gsl
    sudo port -d install gsl-devel

    sudo port -d install python33
    sudo port -d install py33-setuptools
    sudo port -d install py33-ipython
    sudo port -d install py33-py
    sudo port -d install py33-simplejson
    sudo port -d install py33-pip
    sudo port -d install py33-Pillow
    sudo port -d install py33-numpy
    sudo port -d install py33-scipy
    sudo port -d install py33-nose
    sudo port -d install py33-babel
    sudo port -d install py33-django
    sudo port -d install py33-psycopg2
    port installed
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main
