#!/bin/sh
#
########################################################################
# Install Python
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 1/7,2009
#       Stable.
########################################################################

install_python_bzip() {
    mkdir install_python
    cd install_python
    wget http://www.python.org/ftp/python/$1/Python-$1.tar.bz2
    tar xjvf Python-$1.tar.bz2
    cd Python-$1
    test -n "$2" || ./configure
    test -n "$2" && ./configure --prefix $2
    make
    test -n "$2" || sudo make install
    test -n "$2" && make install
    cd ..
    test -d /usr/local/src/python || sudo mkdir -p /usr/local/src/python
    sudo cp -a Python-$1 /usr/local/src/python
    cd ..
    rm -rf install_python
}

test -n "$1" || exit 1
install_python_bzip $1 $2

case $OSTYPE in
  *darwin*)
    sudo chown -R root:wheel /usr/local/src/python
    ;;
  *)
    sudo chown -R root:root /usr/local/src/python
    ;;
esac

python -V
