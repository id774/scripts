#!/bin/sh
#
########################################################################
# Install Cabocha
#  $1 = cabocha version
#  $2 = CRF version
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 10/29,2013
#       Update download URL.
#  v0.1 10/23,2012
#       First.
########################################################################

setup_environment() {
    test -n "$1" || CABOCHA_VERSION=0.66
    test -n "$1" && CABOCHA_VERSION=$1
    test -n "$2" || CRF_VERSION=0.58
    test -n "$2" && CRF_VERSION=$1
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
    sudo mkdir -p /usr/local/src/cabocha
    sudo cp $OPTIONS cabocha-$CABOCHA_VERSION /usr/local/src/cabocha
    sudo mkdir -p /usr/local/src/CRF
    sudo cp $OPTIONS "CRF++-$CRF_VERSION" /usr/local/src/CRF
    sudo chown -R root:root /usr/local/src/cabocha
}

install_cabocha() {
    setup_environment $*

    mkdir install_cabocha
    cd install_cabocha

    wget http://crfpp.googlecode.com/files/CRF%2B%2B-$CRF_VERSION.tar.gz
    tar xzvf "CRF++-$CRF_VERSION.tar.gz"
    cd "CRF++-$CRF_VERSION"
    ./configure
    make
    sudo make install
    cd ..

    wget https://cabocha.googlecode.com/files/cabocha-$CABOCHA_VERSION.tar.bz2
    tar xjvf cabocha-$CABOCHA_VERSION.tar.bz2
    cd cabocha-$CABOCHA_VERSION
    ./configure --with-charset=UTF8 --with-posset=IPA
    make
    sudo make install
    cd python
    python setup.py build_ext
    sudo python setup.py install
    cd ..
    cd ruby
    ruby extconf.rb
    make
    sudo make install
    cd ..
    cd ..

    test -n "$2" || save_sources
    cd ..
    rm -rf install_cabocha
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_cabocha $*
