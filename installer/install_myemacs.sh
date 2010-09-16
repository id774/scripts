#!/bin/sh
#
########################################################################
# Install emacs
#  $1 = prefix
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 9/16,2010
#       First.
########################################################################

setup_environment() {
    EMACS_VERSION=23.2
}

gitpull() {
    echo "Pulling $1 $3"
    if [ -d $HOME/local/$1/$3 ]; then
        cd $HOME/local/$1/$3
        git pull
    else
        cd $HOME/local/$1
        git clone git://github.com/$2/$3.git
    fi
    test -L $HOME/$3 && rm $HOME/$3
    ln -fs $HOME/local/$1/$3 $HOME/$3
}

gitpull_all() {
    test -d $HOME/local/github || mkdir -p $HOME/local/github
    test -d $HOME/local/git || mkdir -p $HOME/local/git
    gitpull github id774-2 emacs
}

make_and_install() {
    test -n "$1" || ./configure --without-x --prefix=$HOME/local/emacs/$EMACS_VERSION
    test -n "$1" && ./configure --without-x --prefix=$1
    make
    $SUDO make install
}

get_emacs() {
    test -d $HOME/local/github/emacs || exit 1
    make_and_install $*
}

install_emacs() {
    setup_environment $*
    get_emacs $*
}

main() {
    gitpull_all
    install_emacs $*
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
main $*
