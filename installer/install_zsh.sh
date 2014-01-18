#!/bin/sh
#
########################################################################
# Install zsh
#  $1 = version
#  $2 = prefix
#  $3 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 1/19,2014
#       Update to zsh 5.0.5, Change source url.
#  v0.2 9/16,2010
#       Refactoring.
#  v0.1 9/14,2010
#       First.
########################################################################

setup_environment() {
    test -n "$1" && ZSH_VERSION=$1
    test -n "$1" || ZSH_VERSION=5.0.5
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

save_sources() {
    test -d /usr/local/src/zsh || sudo mkdir -p /usr/local/src/zsh
    sudo cp $OPTIONS zsh-$ZSH_VERSION /usr/local/src/zsh
    sudo chown $OWNER /usr/local/src/zsh
    sudo chown -R $OWNER /usr/local/src/zsh/zsh-$ZSH_VERSION
}

make_and_install() {
    cd zsh-$ZSH_VERSION
    test -n "$2" || ./configure --prefix=$HOME/local/zsh/$ZSH_VERSION
    test -n "$2" && ./configure --prefix=$2
    make
    $SUDO make install
    cd ..
}

get_zsh() {
    mkdir install_zsh
    cd install_zsh
    wget http://sourceforge.net/projects/zsh/files/zsh/$ZSH_VERSION/zsh-$ZSH_VERSION.tar.gz/download -O zsh-$ZSH_VERSION.tar.gz
    test -f zsh-$ZSH_VERSION.tar.gz || exit 1
    tar xzvf zsh-$ZSH_VERSION.tar.gz
    test "$2" = "sourceonly" || make_and_install $1 $2
    test -n "$3" || save_sources
    cd ..
    rm -rf install_zsh
}

install_zsh() {
    setup_environment $*
    get_zsh $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_zsh $*
