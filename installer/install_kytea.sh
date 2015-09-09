#!/bin/sh
#
########################################################################
# Install KyTea
#  $1 = version
#  $2 = configure prefix
#  $3 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 9/9,2015
#       Install binding.
#  v0.1 9/8,2015
#       First.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=0.4.7
    test -n "$1" && VERSION=$1
    test -n "$3" || SUDO=sudo
    test -n "$3" && SUDO=
    test "$3" = "sudo" && SUDO=sudo
}

save_sources() {
    test -d /usr/local/src/kytea || sudo mkdir -p /usr/local/src/kytea
    sudo cp -a kytea-$VERSION /usr/local/src/kytea
}

install_binding() {
    test -x /opt/ruby/current/bin/gem && $SUDO /opt/ruby/current/bin/gem install kytea
    test -x /opt/python/current/bin/pip && $SUDO /opt/python/current/bin/pip install kytea
}

install_kytea() {
    setup_environment $*
    mkdir install_kytea
    cd install_kytea
    curl -L http://www.phontron.com/kytea/download/kytea-$VERSION.tar.gz -O
    tar xzvf kytea-$VERSION.tar.gz
    cd kytea-$VERSION
    test -n "$2" || ./configure
    test -n "$2" && ./configure --prefix=$2
    make
    $SUDO make install
    cd ../
    test -n "$3" || save_sources
    cd ../
    rm -rf install_kytea
    install_binding $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_kytea $*
