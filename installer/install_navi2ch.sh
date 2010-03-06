#!/bin/sh
#
########################################################################
# Install Navi2ch
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 2/20,2009
#       Recactoring.
#  v0.2 1/19,2009
#       Keep sources.
#  v0.1 1/19,2009
#       First version.
########################################################################

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

main() {
    VER=1.8.1
    SOURCE="http://downloads.sourceforge.net/navi2ch/navi2ch-$VER.tar.gz"
    install_navi2ch
    sudo chown -R $OWNER /usr/local/src/emacs
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

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
main
