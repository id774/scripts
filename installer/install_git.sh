#!/bin/sh
#
########################################################################
# Install git
#  $1 = version
#  $2 = prefix
#  $3 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2014-01-19
#       First.
########################################################################

setup_environment() {
    test -n "$1" && GIT_VERSION=$1
    test -n "$1" || GIT_VERSION=1.8.5.3
    test -n "$3" || SUDO=sudo
    test -n "$3" && SUDO=
    test "$3" = "sudo" && SUDO=sudo
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
    test -d /usr/local/src/git || sudo mkdir -p /usr/local/src/git
    sudo cp $OPTIONS git-$GIT_VERSION /usr/local/src/git
    sudo chown $OWNER /usr/local/src/git
    sudo chown -R $OWNER /usr/local/src/git/git-$GIT_VERSION
}

make_and_install() {
    cd git-$GIT_VERSION
    #test -n "$2" || ./configure --prefix=$HOME/local/git-bin/$GIT_VERSION
    test -n "$2" || ./configure --prefix=/usr/local
    test -n "$2" && ./configure --prefix=$2
    make
    $SUDO make install
    cd ..
}

get_git() {
    mkdir install_git
    cd install_git

    wget http://git-core.googlecode.com/files/git-$GIT_VERSION.tar.gz
    test -f git-$GIT_VERSION.tar.gz || exit 1
    tar xzvf git-$GIT_VERSION.tar.gz

    test "$2" = "sourceonly" || make_and_install $1 $2
    test -n "$3" || save_sources
    cd ..
    rm -rf install_git
}

install_git() {
    setup_environment $*
    get_git $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_git $*
