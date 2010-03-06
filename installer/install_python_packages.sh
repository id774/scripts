#!/bin/sh
#
########################################################################
# Install Python Packages
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 3/1,2010
#       Refactoring.
#  v0.2 2/23,2010
#       Refactoring.
#  v0.1 2/18,2009
#       First version.
########################################################################

install_python_packages() {
    if [ `aptitude search libmysqlclient15-dev | awk '/^i/' | wc -l` = 0 ]; then
        sudo aptitude -y install libmysqlclient15-dev
    fi
    mkdir install_python_packages
    cd install_python_packages
    wget "http://downloads.sourceforge.net/mysql-python/MySQL-python-1.2.2.tar.gz"
    tar xzvf MySQL-python-1.2.2.tar.gz
    cd MySQL-python-1.2.2
    python setup.py build
    sudo python setup.py install
    cd ..
    test -d /usr/local/src/python/packages || sudo mkdir -p /usr/local/src/python/packages
    sudo cp $OPTIONS MySQL-python-1.2.2 /usr/local/src/python/packages/
    sudo chown -R $OWNER /usr/local/src/python/packages
    cd ..
    sudo rm -rf install_python_packages
}

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

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_python_packages
