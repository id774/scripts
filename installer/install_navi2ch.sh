#!/bin/sh
#
########################################################################
# Install Navi2ch
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 1/19,2009
#       Keep sources.
#  v0.1 1/19,2009
#       First version.
########################################################################

set_emacs_permission() {
    case $OSTYPE in
      *darwin*)
        sudo chown -R root:wheel /usr/local/src/emacs
        ;;
      *)
        sudo chown -R root:root /usr/local/src/emacs
        ;;
    esac
}

install_navi2ch() {
    mkdir install_navi2ch
    cd install_navi2ch
    wget $SOURCE
    tar xzvf navi2ch-$VER.tar.gz
    cd navi2ch-$VER
    ./configure
    make
    make check
    sudo make install
    test -d /usr/local/src/emacs/navi2ch || sudo mkdir -p /usr/local/src/emacs/navi2ch
    cd ..
    sudo cp $OPTIONS navi2ch-$VER /usr/local/src/emacs/navi2ch
    cd ..
    rm -rf install_navi2ch
}

case $OSTYPE in
  *darwin*)
    OPTIONS=-pR
    ;;
  *)
    OPTIONS=-a
    ;;
esac

VER=1.8.1
SOURCE="http://downloads.sourceforge.net/navi2ch/navi2ch-$VER.tar.gz"
install_navi2ch
set_emacs_permission

