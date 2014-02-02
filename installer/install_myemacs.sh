#!/bin/sh
#
########################################################################
# Install emacs
#  $1 = prefix
#  $2 = nosudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 3/29,2011
#       Split emacs-w3m.
#  v0.1 9/16,2010
#       First.
########################################################################

setup_environment() {
    EMACS_VERSION=23.2
    test -n "$2" || SUDO=sudo
    test -n "$2" && SUDO=
    test "$2" = "sudo" && SUDO=sudo
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
    gitpull github id774-2 emacs
}

make_and_install() {
    test -n "$1" || ./configure --with-ns --without-x --prefix=$HOME/local/emacs/$EMACS_VERSION
    test -n "$1" && ./configure --with-ns --without-x --prefix=$1
    make
    $SUDO make install
}

install_emacs() {
    test -d $HOME/local/github/emacs || exit 1
    cd $HOME/local/github/emacs
    make_and_install $*
}

main() {
    setup_environment $*
    gitpull_all
    install_emacs $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*
