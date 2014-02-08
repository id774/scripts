#!/bin/sh
#
########################################################################
# Install Python
#  $1 = version
#  $2 = prefix
#  $3 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.5 2/9,2014
#       Easy install as default, create symlink, bug fix.
#  v1.4 9/16,2010
#       Refactoring.
#  v1.3 3/7,2010
#       Refactoring.
#  v1.2 2/20,2010
#       Refactoring.
#  v1.1 2/21,2009
#       Add sourceonly option.
#  v1.0 1/7,2009
#       Stable.
########################################################################

setup_environment() {
    test -n "$2" || PREFIX=/usr/local
    test -n "$2" && PREFIX=$2
    test -n "$3" || SUDO=sudo
    test -n "$3" && SUDO=
    test "$3" = "sudo" && SUDO=sudo
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
    test -d /usr/local/src/python || $SUDO mkdir -p /usr/local/src/python
    $SUDO cp $OPTIONS Python-$1 /usr/local/src/python
    $SUDO chown $OWNER /usr/local/src/python
    $SUDO chown -R $OWNER /usr/local/src/python/Python-$1
}

make_and_install() {
    cd Python-$1
    ./configure --prefix $PREFIX
    make
    $SUDO make install
    cd ..
}

get_python() {
    mkdir install_python
    cd install_python
    wget http://www.python.org/ftp/python/$1/Python-$1.tar.bz2
    test -f Python-$1.tar.bz2 || exit 1
    tar xjvf Python-$1.tar.bz2
    test "$2" = "sourceonly" || make_and_install $*
    test -n "$3" || save_sources $*
    cd ..
    $SUDO rm -rf install_python
}

create_symlink() {
    test -x $PREFIX/bin/python3 && test -x $PREFIX/bin/python || $SUDO ln -s $PREFIX/bin/python3 $PREFIX/bin/python
}

get_easy_install() {
    wget https://bitbucket.org/pypa/setuptools/raw/bootstrap/ez_setup.py -O - | $SUDO $PREFIX/bin/python
}

get_pip() {
    wget https://raw.github.com/pypa/pip/master/contrib/get-pip.py -O - | $SUDO $PREFIX/bin/python
}

install_python() {
    setup_environment $*
    test -n "$1" || exit 1
    get_python $*
    create_symlink $*
    get_easy_install $*
    get_pip $*
    python -V
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_python $*
