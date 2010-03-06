#!/bin/sh
#
########################################################################
# Install Python
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 2/20,2010
#       Refactoring.
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
    sudo make install
    cd ..
}

get_python() {
    mkdir install_python
    cd install_python
    wget http://www.python.org/ftp/python/$1/Python-$1.tar.bz2
    test -f Python-$1.tar.bz2 || exit 1
    tar xjvf Python-$1.tar.bz2
    test "$2" = "sourceonly" || make_and_install $1 $2
    test -d /usr/local/src/python || sudo mkdir -p /usr/local/src/python
    sudo cp $OPTIONS Python-$1 /usr/local/src/python
    sudo chown $OWNER /usr/local/src/python
    sudo chown -R $OWNER /usr/local/src/python/Python-$1
    cd ..
    rm -rf install_python
}

main() {
    test -n "$1" || exit 1
    get_python $1 $2

    python -V
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

ping -c 1 -i 3 google.com > /dev/null 2>&1 && main
