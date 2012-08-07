#!/bin/sh
#
########################################################################
# Install Mecab
#  $1 = mecab version
#  $2 = ipadic version
#  $3 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 8/7,2012
#       First.
########################################################################

setup_environment() {
    test -n "$1" || MECAB_VERSION=0.994
    test -n "$1" && MECAB_VERSION=$1
    test -n "$2" || IPADIC_VERSION=2.7.0-20070801
    test -n "$2" && IPADIC_VERSION=$2
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

save_sources() {
    sudo mkdir -p /usr/local/src/mecab
    sudo cp $OPTIONS mecab-$MECAB_VERSION /usr/local/src/mecab
    sudo cp $OPTIONS mecab-ruby-$MECAB_VERSION /usr/local/src/mecab
    sudo cp $OPTIONS mecab-ipadic-$IPADIC_VERSION /usr/local/src/mecab
    sudo chown -R root:root /usr/local/src/mecab
}

install_mecab() {
    setup_environment $*

    mkdir install_mecab
    cd install_mecab

    wget http://mecab.googlecode.com/files/mecab-$MECAB_VERSION.tar.gz
    tar xzvf mecab-$MECAB_VERSION.tar.gz
    cd mecab-$MECAB_VERSION
    ./configure
    make
    sudo make install
    cd ..
    wget http://mecab.googlecode.com/files/mecab-ruby-$MECAB_VERSION.tar.gz
    tar xzvf mecab-ruby-$MECAB_VERSION.tar.gz
    cd mecab-ruby-$MECAB_VERSION
    ruby extconf.rb
    make
    sudo make install
    cd ..
    wget http://mecab.googlecode.com/files/mecab-ipadic-$IPADIC_VERSION.tar.gz
    tar xzvf mecab-ipadic-$IPADIC_VERSION.tar.gz
    cd mecab-ipadic-$IPADIC_VERSION
    ./configure --with-charset=utf8
    make
    sudo make install
    cd ..

    test -n "$2" || save_sources
    cd ..
    rm -rf install_mecab
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_mecab $*
