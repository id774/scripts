#!/bin/sh
#
########################################################################
# Install Navi2ch
#  $1 = version
#  $2 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.5 2010-09-16
#       Refactoring.
#  v0.4 2010-03-07
#       Refactoring and update to 1.8.3.
#  v0.3 2009-02-20
#       Recactoring.
#  v0.2 2009-01-19
#       Keep sources.
#  v0.1 2009-01-19
#       First version.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=1.8.4
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
    test -n "$2" || save_sources
    cd ..
    rm -rf install_navi2ch
}

main() {
    setup_environment $*
    install_navi2ch $*
    sudo chown -R $OWNER /usr/local/src/emacs
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*
