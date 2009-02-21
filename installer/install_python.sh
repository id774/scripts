#!/bin/sh
#
########################################################################
# Install Python
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 2/21,2009
#       Add sourceonly option.
#  v1.0 1/7,2009
#       Stable.
########################################################################

make_and_install() {
    cd Python-$1
    test -n "$2" || ./configure
    test -n "$2" && ./configure --prefix $2
    make
    test -n "$2" || sudo make install
    test -n "$2" && make install
    cd ..
}

get_python() {
    mkdir install_python
    cd install_python
    wget http://www.python.org/ftp/python/$1/Python-$1.tar.bz2
    test -f Python-$1.tar.bz2 || exit 1
    tar xjvf Python-$1.tar.bz2
    test "$2" = "sourceonly" || make_and_install
    test -d /usr/local/src/python || sudo mkdir -p /usr/local/src/python
    sudo cp $OPTIONS Python-$1 /usr/local/src/python
    cd ..
    rm -rf install_python
}

case $OSTYPE in
  *darwin*)
    OPTIONS=-pR
    ;;
  *)
    OPTIONS=-a
    ;;
esac

test -n "$1" || exit 1
get_python $1 $2

case $OSTYPE in
  *darwin*)
    sudo chown -R root:wheel /usr/local/src/python
    ;;
  *)
    sudo chown -R root:root /usr/local/src/python
    ;;
esac

python -V
