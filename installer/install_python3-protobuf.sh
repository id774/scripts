#!/bin/sh
#
########################################################################
# Install Python 3 Protocol Buffers
#  $1 = prefix
#  $2 = nosudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 4/11,2014
#       First.
########################################################################

setup_environment() {
    test -n "$2" || SUDO=sudo
    test -n "$2" && SUDO=
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
    test -n "$2" || ./configure --prefix=/opt/protobuf
    test -n "$2" && ./configure --prefix=$1
    make
    make check
    $SUDO make install
}

install_trunk() {
    gitpull_all
    test -d $HOME/local/github/python3-protobuf || exit 1
    cd $HOME/local/github/python3-protobuf
    make_and_install $*
}

main() {
    setup_environment $*
    case "$1" in
      trunk)
        install_trunk $*
        ;;
      *)
        ;;
    esac
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*
