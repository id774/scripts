#!/bin/sh
#
########################################################################
# Install CoffeeScript
#  $1 = prefix
#  $2 = nosudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 1/23,2012
#       Install coffee only.
#  v0.2 12/1,2011
#       Add npm.
#  v0.1 9/15,2011
#       First.
########################################################################

setup_environment() {
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
    gitpull github jashkenas coffee-script
}

make_and_install_coffee() {
    $SUDO bin/cake install
}

install_coffee() {
    test -d $HOME/local/github/coffee-script || exit 1
    cd $HOME/local/github/coffee-script
    make_and_install_coffee $*
}

main() {
    setup_environment $*
    gitpull_all
    install_coffee $*
    coffee -v
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*
