#!/bin/sh
#
########################################################################
# Install Des
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 3/7,2010
#       Refactoring.
#  v1.0 5/21,2009
#       Derived from install_crypt.sh.
########################################################################

setup_environment() {
    case $OSTYPE in
      *darwin*)
        OWNER=root:root
        ;;
      *)
        OWNER=root:wheel
        ;;
    esac
}

install_des() {
    setup_environment
    mkdir install_des
    cd install_des
    wget http://id774.net/archive/kmdes-ubuntu.tar.gz
    md5.sh kmdes-ubuntu.tar.gz
    tar xzvf kmdes-ubuntu.tar.gz
    rm kmdes-ubuntu.tar.gz
    cd des
    test -d /usr/local/src/crypt/des && sudo rm -rf /usr/local/src/crypt/des
    sudo mkdir -p /usr/local/src/crypt/des
    sudo cp * /usr/local/src/crypt/des
    make
    sudo make install
    cd ..
    rm -rf des
    cd ..
    rm -rf install_des
    sudo chown -R $OWNER /usr/local/src/crypt/des
    set_des_permission
}

main() {
    which dmsetup > /dev/null || sudo aptitude -y install dmsetup
    which des > /dev/null || install_des
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
main
