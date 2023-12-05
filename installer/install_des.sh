#!/bin/sh
#
########################################################################
# Install Des
#  $1 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.3 2011-09-30
#       Rename tar filename.
#  v1.2 2010-09-16
#       Refactoring.
#  v1.1 2010-03-07
#       Refactoring.
#  v1.0 2009-05-21
#       Derived from install_crypt.sh.
########################################################################

setup_environment() {
    case $OSTYPE in
      *darwin*)
        OWNER=root:wheel
        ;;
      *)
        OWNER=root:root
        ;;
    esac
}

save_sources() {
    test -d /usr/local/src/crypt/des && sudo rm -rf /usr/local/src/crypt/des
    sudo mkdir -p /usr/local/src/crypt/des
    sudo cp * /usr/local/src/crypt/des
    sudo chown -R $OWNER /usr/local/src/crypt/des
}

install_des() {
    setup_environment
    mkdir install_des
    cd install_des
    wget http://id774.net/archive/kmdes.tar.gz
    md5.sh kmdes.tar.gz
    tar xzvf kmdes.tar.gz
    rm kmdes.tar.gz
    cd des
    test -n "$1" || save_sources
    make
    sudo make install
    cd ..
    rm -rf des
    cd ..
    rm -rf install_des
}

main() {
    which dmsetup > /dev/null || sudo apt-get -y install dmsetup
    which des > /dev/null || install_des $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*
