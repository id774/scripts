#!/bin/sh
#
########################################################################
# Install Mew
#  $1 = emacs path
#  $2 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 2011-12-08
#       The deployment files owned by root.
#  v0.1 2011-03-29
#       First.
########################################################################

setup_environment() {
    VERSION=6.5
    case $OSTYPE in
      *darwin*)
        OWNER=root:wheel
        ;;
      *)
        OWNER=root:root
        ;;
    esac
}

save_sources() {
    test -d /usr/local/src/emacs/mew-$VERSION && sudo rm -rf /usr/local/src/emacs/mew-$VERSION
    test -d /usr/local/src/emacs || sudo mkdir -p /usr/local/src/emacs
    sudo cp -R mew-$VERSION /usr/local/src/emacs/
    sudo chown -R $OWNER /usr/local/src/emacs
}

install_mew() {
    setup_environment
    mkdir install_mew
    cd install_mew
    wget http://www.mew.org/Release/mew-$VERSION.tar.gz
    tar xzvf mew-$VERSION.tar.gz
    rm mew-$VERSION.tar.gz
    cd mew-$VERSION
    ./configure --with-elispdir=$HOME/.emacs.d/elisp/3rd-party \
    --with-etcdir=$HOME/.emacs.d/elisp/3rd-party \
    --with-emacs=$1
    make
    make info
    make jinfo
    sudo make install
    sudo make install-jinfo
    cd ..
    test -n "$2" || save_sources $*
    cd ..
    rm -rf install_mew
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
test -n "$1" || exit 1
install_mew $*
