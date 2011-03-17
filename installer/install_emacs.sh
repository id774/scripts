#!/bin/sh
#
########################################################################
# Install emacs
#  $1 = version
#  $2 = prefix
#  $3 = build options
#  $4 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 3/17,2011
#       Emacs 23.3.
#  v0.2 9/16,2010
#       Refactoring.
#  v0.1 9/14,2010
#       First.
########################################################################

setup_environment() {
    test -n "$1" && EMACS_VERSION=$1
    test -n "$1" || EMACS_VERSION=23.3
    test -n "$3" && BUILD_OPTIONS=$3
    test -n "$3" || BUILD_OPTIONS=--with-ns
    test -n "$4" || SUDO=sudo
    test -n "$4" && SUDO=
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

adding_patches() {
    wget http://cloud.github.com/downloads/typester/emacs/feature-fullscreen.patch
    patch -p < feature-fullscreen.patch
    wget http://cloud.github.com/downloads/typester/emacs/fix-shiftmodifier-with-ime.patch
    patch -p1 < fix-shiftmodifier-with-ime.patch
}

make_and_install() {
    cd emacs-$EMACS_VERSION
    adding_patches $*
    test -n "$2" || ./configure --without-x $3 --prefix=$HOME/local/emacs/$EMACS_VERSION
    test -n "$2" && ./configure --without-x $3 --prefix=$2
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
    test "$2" = "sourceonly" || make_and_install $*
    test -n "$4" || save_sources
    cd ..
    rm -rf install_emacs
}

install_emacs() {
    setup_environment $*
    get_emacs $*
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_emacs $*
