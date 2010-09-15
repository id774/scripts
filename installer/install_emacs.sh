#!/bin/sh
#
########################################################################
# Install emacs
#  $1 = version
#  $2 = prefix
#  $3 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 9/16,2010
#       Refactoring.
#  v0.1 9/14,2010
#       First.
########################################################################

setup_environment() {
    test -n "$1" && EMACS_VERSION=$1
    test -n "$1" || EMACS_VERSION=23.2
    test -n "$2" || ./configure --without-x --prefix=$HOME/local/emacs/$EMACS_VERSION
    test -n "$2" && ./configure --without-x --prefix=$2
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
    test -d /usr/local/src/emacs || sudo mkdir -p /usr/local/src/emacs
    sudo cp $OPTIONS emacs-$EMACS_VERSION /usr/local/src/emacs
    sudo chown $OWNER /usr/local/src/emacs
    sudo chown -R $OWNER /usr/local/src/emacs/emacs-$EMACS_VERSION
}

make_and_install() {
    cd emacs-$EMACS_VERSION
    test -n "$2" || ./configure --without-x --prefix=$HOME/local/emacs/$EMACS_VERSION
    test -n "$2" && ./configure --without-x --prefix=$2
    make
    $SUDO make install
    cd ..
}

get_emacs() {
    mkdir install_emacs
    cd install_emacs
    wget ftp://ftp.ring.gr.jp/pub/GNU/emacs/emacs-$EMACS_VERSION.tar.bz2
    test -f emacs-$EMACS_VERSION.tar.bz2 || exit 1
    tar xjvf emacs-$EMACS_VERSION.tar.bz2
    test "$2" = "sourceonly" || make_and_install $1 $2
    test -n "$3" || save_sources
    cd ..
    rm -rf install_emacs
}

install_emacs() {
    setup_environment $*
    get_emacs $*
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_emacs $*
