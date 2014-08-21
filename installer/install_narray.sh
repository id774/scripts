#!/bin/sh
#
########################################################################
# Install NArray
#  $1 = ruby path (ex. /opt/bin)
#  $2 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 10/17,2012
#       First.
########################################################################

setup_environment() {
    test -n "$1" && export RUBY=$1/bin/ruby
    test -n "$1" || export RUBY=/opt/ruby/current/bin/ruby
    test -n "$2" || SUDO=sudo
    test -n "$2" && SUDO=
    test "$2" = "sudo" && SUDO=sudo
    case $OSTYPE in
      *darwin*)
        OWNER=root:wheel
        OPTIONS=-pR
        ;;
      *)
        OWNER=root:root
        OPTIONS=-a
        ;;
    esac
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
    gitpull github masa16 narray
}

make_and_install_narray() {
    $RUBY extconf.rb
    make
    $SUDO make install
}

install_narray() {
    setup_environment $*
    gitpull_all
    test -d $HOME/local/github/narray || exit 1
    cd $HOME/local/github/narray
    make_and_install_narray $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_narray $*
