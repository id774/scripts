#!/bin/sh
#
########################################################################
# Install zsh
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 9/14,2010
#       First.
########################################################################

make_and_install() {
    cd zsh-$ZSH_VERSION
    test -n "$2" || ./configure --prefix=$HOME/local/zsh/$ZSH_VERSION
    test -n "$2" && ./configure --prefix=$2
    make
    sudo make install
    cd ..
}

get_zsh() {
    mkdir install_zsh
    cd install_zsh
    wget wget http://www.zsh.org/pub/zsh-$ZSH_VERSION.tar.gz
    test -f zsh-$ZSH_VERSION.tar.gz || exit 1
    tar xzvf zsh-$ZSH_VERSION.tar.gz
    test "$2" = "sourceonly" || make_and_install $1 $2
    test -d /usr/local/src/zsh || sudo mkdir -p /usr/local/src/zsh
    sudo cp $OPTIONS zsh-$ZSH_VERSION /usr/local/src/zsh
    sudo chown $OWNER /usr/local/src/zsh
    sudo chown -R $OWNER /usr/local/src/zsh/zsh-$ZSH_VERSION
    cd ..
    rm -rf install_zsh
}

setup_environment() {
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

install_zsh() {
    setup_environment
    test -n "$1" && ZSH_VERSION=$1
    test -n "$1" || ZSH_VERSION=4.3.10
    get_zsh $1 $2
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_zsh $*
