#!/bin/sh
#
########################################################################
# Install Gauche
#  $1 = version
#  $2 = prefix
#  $3 = nosudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 5/16,2012
#       First.
########################################################################

setup_environment() {
    test -n "$3" || SUDO=sudo
    test -n "$3" && SUDO=
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

gitpull() {
    echo "Pulling $1 $2"
    if [ -d $HOME/local/$1/$2 ]; then
        cd $HOME/local/$1/$2
        git pull
    else
        cd $HOME/local/$1
        git clone git://gauche.git.sourceforge.net/gitroot/gauche/Gauche
    fi
    test -L $HOME/$2 && rm $HOME/$2
    ln -fs $HOME/local/$1/$2 $HOME/$2
}

gitpull_all() {
    test -d $HOME/local/git || mkdir -p $HOME/local/git
    gitpull git Gauche
}

make_and_install() {
    ./DIST gen
    test -n "$2" || ./configure --prefix=$HOME/local/gauche/trunk
    test -n "$2" && ./configure --prefix=$2
    make
    $SUDO make install
}

install_trunk() {
    gitpull_all
    test -d $HOME/local/git/Gauche || exit 1
    cd $HOME/local/git/Gauche
    make_and_install $*
}

install_stable() {
    mkdir install_gauche
    cd install_gauche
    wget http://prdownloads.sourceforge.net/gauche/Gauche-$1.tgz
    tar xzvf Gauche-$1.tgz
    cd Gauche-$1
    make_and_install $*
    cd ..
    test -d /usr/local/src/gauche || sudo mkdir -p /usr/local/src/gauche
    sudo cp $OPTIONS Gauche-$1 /usr/local/src/gauche
    sudo chown -R $OWNER /usr/local/src/gauche/Gauche-$1
    cd ..
    sudo rm -rf install_gauche
}

main() {
    setup_environment $*
    case "$1" in
      0.9.3.2)
        install_stable $*
        ;;
      trunk)
        install_trunk $*
        ;;
      *)
        ;;
    esac
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*
