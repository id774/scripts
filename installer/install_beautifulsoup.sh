#!/bin/sh
#
########################################################################
# Install BeautifulSoup
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 8/30,2010
#       First version.
########################################################################

setup_environment() {
    test -n "$1" || BEAUTIFULSOUP_VERSION=3.0.8.1
    test -n "$1" && BEAUTIFULSOUP_VERSION=$1
    test -n "$2" && export PYTHON=$2
    test -n "$2" || export PYTHON=python
}

install_beautifulsoup() {
    setup_environment $*

    mkdir install_beautifulsoup
    cd install_beautifulsoup

    wget http://www.crummy.com/software/BeautifulSoup/download/3.x/BeautifulSoup-$BEAUTIFULSOUP_VERSION.tar.gz
    tar xzvf BeautifulSoup-$BEAUTIFULSOUP_VERSION.tar.gz
    cd BeautifulSoup-$BEAUTIFULSOUP_VERSION
    sudo $PYTHON setup.py install

    cd ../..
    rm -rf install_beautifulsoup
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_beautifulsoup $*
