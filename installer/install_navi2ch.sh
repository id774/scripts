#!/bin/sh
#
########################################################################
# Install Navi2ch
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.4 3/7,2010
#       Refactoring and update to 1.8.3.
#  v0.3 2/20,2009
#       Recactoring.
#  v0.2 1/19,2009
#       Keep sources.
#  v0.1 1/19,2009
#       First version.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=1.8.3
    test -n "$1" && VERSION=$1
    SOURCE="http://downloads.sourceforge.net/navi2ch/navi2ch-$VERSION.tar.gz"

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
    test -d /usr/local/src/emacs/navi2ch || sudo mkdir -p /usr/local/src/emacs/navi2ch
    sudo cp $OPTIONS navi2ch-$VERSION /usr/local/src/emacs/navi2ch
}

install_navi2ch() {
    mkdir install_navi2ch
    cd install_navi2ch
    wget $SOURCE
    tar xzvf navi2ch-$VERSION.tar.gz
    cd navi2ch-$VERSION
    ./configure
    make
    make check
    sudo make install
    cd ..
    save_sources
    cd ..
    rm -rf install_navi2ch
}

main() {
    setup_environment $*
    install_navi2ch
    sudo chown -R $OWNER /usr/local/src/emacs
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
main $*
