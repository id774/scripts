#!/bin/sh
#
########################################################################
# Install Python Packages
#  $1 = version
#  $2 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.5 9/16,2010
#       Refactoring.
#  v0.4 3/7,2010
#       Refactoring.
#  v0.3 3/1,2010
#       Refactoring.
#  v0.2 2/23,2010
#       Refactoring.
#  v0.1 2/18,2009
#       First version.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=1.2.2
    test -n "$1" && VERSION=$1
    case $OSTYPE in
      *darwin*)
        OPTIONS=-pR
        OWNER=root:wheel
        ;;
      *)
        OPTIONS=-a
        OWNER=root:root
        ;;
    esac
}

save_sources() {
    test -d /usr/local/src/python/packages || sudo mkdir -p /usr/local/src/python/packages
    sudo cp $OPTIONS MySQL-python-$VERSION /usr/local/src/python/packages/
    sudo chown -R $OWNER /usr/local/src/python/packages
}

install_python_packages() {
    setup_environment $*
    sudo apt-get -y install libmysqlclient15-dev
    mkdir install_python_packages
    cd install_python_packages
    wget "http://downloads.sourceforge.net/mysql-python/MySQL-python-$VERSION.tar.gz"
    tar xzvf MySQL-python-$VERSION.tar.gz
    cd MySQL-python-$VERSION
    python setup.py build
    sudo python setup.py install
    cd ..
    test -n "$2" || save_sources
    cd ..
    sudo rm -rf install_python_packages
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_python_packages $*
