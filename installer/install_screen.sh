#!/bin/sh
#
########################################################################
# Install screen
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 9/14,2010
#       First.
########################################################################

setup_environment() {
    test -n "$2" || SUDO=
    test -n "$2" && SUDO=sudo
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
    test -d /usr/local/src/screen || sudo mkdir -p /usr/local/src/screen
    sudo cp $OPTIONS screen-$SCREEN_VERSION /usr/local/src/screen
    sudo chown $OWNER /usr/local/src/screen
    sudo chown -R $OWNER /usr/local/src/screen/screen-$SCREEN_VERSION
}

make_and_install() {
    cd screen-$SCREEN_VERSION
    test -n "$2" || ./configure --prefix=$HOME/local/screen/$SCREEN_VERSION
    test -n "$2" && ./configure --prefix=$2
    make
    $SUDO make install
    cd ..
}

get_screen() {
    mkdir install_screen
    cd install_screen
    wget wget ftp://www.dekaino.net/pub/screen/screen-$SCREEN_VERSION.tar.gz
    test -f screen-$SCREEN_VERSION.tar.gz || exit 1
    tar xzvf screen-$SCREEN_VERSION.tar.gz
    test "$2" = "sourceonly" || make_and_install $1 $2
    save_sources
    cd ..
    rm -rf install_screen
}

install_screen() {
    setup_environment $*
    test -n "$1" && SCREEN_VERSION=$1
    test -n "$1" || SCREEN_VERSION=4.0.3
    get_screen $1 $2
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_screen $*
