#!/bin/sh
#
########################################################################
# Install Mecab
#  $1 = mecab version
#  $2 = ipadic version
#  $3 = naistdic version
#  $4 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.4 5/29,2015
#       Change URL, fix args bug.
#  v0.3 2/10,2014
#       Refactoring.
#  v0.2 1/28,2013
#       Add NAIST dic.
#  v0.1 8/7,2012
#       First.
########################################################################

setup_environment() {
    test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
    test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts
    test -n "$1" || MECAB_VERSION=0.996
    test -n "$1" && MECAB_VERSION=$1
    test -n "$2" || IPADIC_VERSION=2.7.0-20070801
    test -n "$2" && IPADIC_VERSION=$2
    test -n "$3" || NAISTDIC_NUM=53500
    test -n "$3" && NAISTDIC_NUM=$4
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
    sudo cp $OPTIONS mecab-python-$MECAB_VERSION /usr/local/src/mecab
    sudo cp $OPTIONS mecab-ipadic-$IPADIC_VERSION /usr/local/src/mecab
    sudo cp $OPTIONS mecab-naist-jdic-$NAISTDIC_VERSION /usr/local/src/mecab
    sudo chown -R $OWNER /usr/local/src/mecab
}

source_compile() {
    wget http://files.id774.net/archive/mecab-$MECAB_VERSION.tar.gz
    tar xzvf mecab-$MECAB_VERSION.tar.gz
    cd mecab-$MECAB_VERSION
    ./configure --enable-utf8-only
    make
    sudo make install
    cd ..
}

ipadic_compile() {
    wget http://files.id774.net/archive/mecab-ipadic-$IPADIC_VERSION.tar.gz
    tar xzvf mecab-ipadic-$IPADIC_VERSION.tar.gz
    cd mecab-ipadic-$IPADIC_VERSION
    ./configure --with-charset=utf8
    make
    sudo make install
    cd ..
}

naistdic_compile() {
    wget http://files.id774.net/archive/naistdic.tar.gz
    tar xzvf naistdic.tar.gz
    cd mecab-naist-jdic-$NAISTDIC_VERSION
    ./configure --with-charset=utf8
    make
    sudo make install
    cd ..
}

get_binding() {
    wget http://files.id774.net/archive/mecab-python-$MECAB_VERSION.tar.gz
    tar xzvf mecab-python-$MECAB_VERSION.tar.gz
    wget http://files.id774.net/archive/mecab-ruby-$MECAB_VERSION.tar.gz
    tar xzvf mecab-ruby-$MECAB_VERSION.tar.gz
}

install_mecab() {
    mkdir install_mecab
    cd install_mecab

    source_compile $*
    ipadic_compile $*
    naistdic_compile $*
    get_binding $*

    test -n "$4" || save_sources
    cd ..
    rm -rf install_mecab
}

install_binding() {
    # $SCRIPTS/installer/install_mecab_ruby.sh /opt/ruby/current /usr/local/src/mecab/mecab-ruby-$MECAB_VERSION
    $SCRIPTS/installer/install_mecab_python.sh /opt/python/current /usr/local/src/mecab/mecab-python-$MECAB_VERSION
}

main() {
    setup_environment $*
    install_mecab $*
    test -n "$5" || install_binding $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*
