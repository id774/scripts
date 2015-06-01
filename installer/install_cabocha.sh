#!/bin/sh
#
########################################################################
# Install CaboCha
#  $1 = cabocha version
#  $2 = CRF version
#  $3 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.7 6/1,2015
#       Specify mecab-config.
#  v0.6 5/29,2015
#       Change URL, fix args bug.
#  v0.5 2/10,2014
#       Refactoring.
#  v0.4 1/27,2014
#       Fix erase process.
#  v0.3 1/19,2014
#       Not use https.
#  v0.2 10/29,2013
#       Update download URL.
#  v0.1 10/23,2012
#       First.
########################################################################

setup_environment() {
    test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
    test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts
    test -n "$1" || CABOCHA_VERSION=0.67
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

install_crf() {
    wget http://files.id774.net/archive/CRF%2B%2B-$CRF_VERSION.tar.gz
    tar xzvf "CRF++-$CRF_VERSION.tar.gz"
    cd "CRF++-$CRF_VERSION"
    ./configure
    make
    sudo make install
    cd ..
}

install_cabocha() {
    wget http://files.id774.net/archive/cabocha-$CABOCHA_VERSION.tar.bz2
    tar xjvf cabocha-$CABOCHA_VERSION.tar.bz2
    cd cabocha-$CABOCHA_VERSION
    ./configure --with-charset=UTF8 --with-posset=IPA --with-mecab-config=`which mecab-config`
    make
    sudo make install
}

install_crf_and_cabocha() {
    mkdir install_cabocha
    cd install_cabocha

    install_crf $*
    install_cabocha $*

    cd ..
    test -n "$3" || save_sources
    cd ..
    sudo rm -rf install_cabocha
}

install_binding() {
    $SCRIPTS/installer/install_cabocha_ruby.sh /opt/ruby/current /usr/local/src/cabocha/cabocha-$CABOCHA_VERSION/ruby
    $SCRIPTS/installer/install_cabocha_python.sh /opt/python/current /usr/local/src/cabocha/cabocha-$CABOCHA_VERSION/python
}

main() {
    setup_environment $*
    install_crf_and_cabocha $*
    install_binding $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*
