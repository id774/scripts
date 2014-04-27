#!/bin/sh
#
########################################################################
# Install Python 3 Protocol Buffers
#  $1 = protobuf prefix
#  $2 = python path
#  $3 = nosudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 4/12,2014
#       Add python binding.
#  v0.1 4/11,2014
#       First.
########################################################################

setup_environment() {
    test -n "$1" && PREFIX=$1
    test -n "$1" || PREFIX=/opt/protobuf/trunk
    test -n "$2" && PYTHON_BIN=$2/bin/python
    test -n "$2" || PYTHON_BIN=/opt/python/current/bin/python
    test -n "$3" || SUDO=sudo
    test -n "$3" && SUDO=
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

gitpull() {
    echo "Pulling $1 $2"
    if [ -d $HOME/local/$1/$2 ]; then
        cd $HOME/local/$1/$2
        git pull
    else
        cd $HOME/local/$1
        git clone git://github.com/openx/python3-protobuf.git
    fi
    test -L $HOME/$2 && rm $HOME/$2
    ln -fs $HOME/local/$1/$2 $HOME/$2
}

gitpull_all() {
    test -d $HOME/local/github || mkdir -p $HOME/local/git
    gitpull github python3-protobuf
}

make_and_install() {
    ./autogen.sh
    ./configure --prefix=$PREFIX
    make
    make check
    $SUDO make install
}

install_python_binding() {
    cd python
    $PYTHON_BIN setup.py build
    $PYTHON_BIN setup.py test
    $SUDO $PYTHON_BIN setup.py install
}

install_trunk() {
    gitpull_all
    test -d $HOME/local/github/python3-protobuf || exit 1
    cd $HOME/local/github/python3-protobuf
    make_and_install $*
    install_python_binding $*
}

main() {
    setup_environment $*
    install_trunk $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*
