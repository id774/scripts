#!/bin/sh
#
########################################################################
# Install TrueCrypt
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.5 8/7,2010
#       Obsolated.
#  v1.4 3/7,2010
#       Refactoring.
#  v1.3 12/23,2009
#       Add new option 6.3a.
#  v1.2.2 12/3,2009
#       Update to 6.3a.
#  v1.2.1 10/23,2009
#       Update to 6.3, and refactoring.
#  v1.2 5/21,2009
#       Refactoring truecrypt installer.
#  v1.1 1/18,2009
#       Update to 6.1a, and refactoring.
#  v1.0 8/15,2008
#       Stable.
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

set_truecrypt_permission() {
    sudo chown -R $OWNER /usr/local/src/crypt/truecrypt
    sudo chown $OWNER /usr/local/src/crypt
    sudo chown $OWNER /usr/local/src
}

setup_truecrypt_rpm() {
    sudo rpm -Uvh $1
    sudo mv $1 /usr/local/src/crypt/truecrypt
    sudo chmod 644 /usr/local/src/crypt/truecrypt/$1
}

setup_truecrypt_deb() {
    sudo dpkg -i $1
    sudo mv $1 /usr/local/src/crypt/truecrypt
    sudo chmod 644 /usr/local/src/crypt/truecrypt/$1
}

install_truecrypt_rpm() {
    wget http://id774.net/truecrypt/$1
    md5.sh $1
    setup_truecrypt_rpm $1
}

install_truecrypt_deb() {
    wget http://id774.net/truecrypt/$1
    md5.sh $1
    setup_truecrypt_deb $1
}

install_truecrypt() {
    mkdir install_truecrypt
    cd install_truecrypt
    case "$1" in
      i386_deb)
        install_truecrypt_deb truecrypt_$TRUECRYPT_CURRENT_VERSION-0_i386.deb
        ;;
      amd64_deb)
        install_truecrypt_deb truecrypt_$TRUECRYPT_CURRENT_VERSION-0_amd64.deb
        ;;
      i586_rpm)
        install_truecrypt_rpm truecrypt-$TRUECRYPT_CURRENT_VERSION-0.i586.rpm
        ;;
      x86_64_rpm)
        install_truecrypt_rpm truecrypt-$TRUECRYPT_CURRENT_VERSION-0.x86_64.rpm
        ;;
      win)
        sudo wget "http://id774.net/truecrypt/TrueCrypt Setup $TRUECRYPT_CURRENT_VERSION.exe"
        sudo mv * /usr/local/src/crypt/truecrypt
        ;;
      macosx)
        sudo wget "http://id774.net/truecrypt/TrueCrypt $TRUECRYPT_CURRENT_VERSION Mac OS X.dmg"
        sudo mv * /usr/local/src/crypt/truecrypt
        ;;
      source)
        sudo wget "http://id774.net/truecrypt/TrueCrypt $TRUECRYPT_CURRENT_VERSION Source.tar.gz"
        sudo mv * /usr/local/src/crypt/truecrypt
        ;;
    esac
    cd ..
    rm -rf install_truecrypt
    set_truecrypt_permission
}

install_crypt_main() {
    TRUECRYPT_CURRENT_VERSION=6.3a
    setup_environment
    which dmsetup > /dev/null || sudo apt-get -y install dmsetup
    test -d /usr/local/src/crypt/truecrypt || sudo mkdir -p /usr/local/src/crypt/truecrypt
    install_truecrypt $1
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_crypt_main $*
