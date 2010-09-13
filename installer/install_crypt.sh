#!/bin/sh
#
########################################################################
# Install TrueCrypt 7
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 8/7,2010
#       Stable.
########################################################################
install_truecrypt() {
    mkdir install_truecrypt
    cd install_truecrypt
    case "$1" in
      linux-i386)
        purge_old_version
        wget "http://id774.net/truecrypt/truecrypt-$2-linux-console-x86.tar.gz"
        sudo cp truecrypt-$2-linux-console-x86.tar.gz /usr/local/src/crypt/truecrypt
        tar xzvf truecrypt-$2-linux-console-x86.tar.gz
        ./truecrypt-$2-setup-console-x86
        if [ -f $TMP/truecrypt_$2_console_i386.tar.gz ]; then
            cd $TMP
            tar xzvf truecrypt_$2_console_i386.tar.gz
            rm truecrypt_$2_console_i386.tar.gz 
            test -d usr || exit 1
            sudo cp -Rv usr /
            rm -rf usr
        fi
        file /usr/bin/truecrypt
        set_truecrypt_permission
        ;;
      linux-amd64)
        purge_old_version
        wget "http://id774.net/truecrypt/truecrypt-$2-linux-console-x64.tar.gz"
        sudo cp truecrypt-$2-linux-console-x64.tar.gz /usr/local/src/crypt/truecrypt
        tar xzvf truecrypt-$2-linux-console-x64.tar.gz
        ./truecrypt-$2-setup-console-x64
        if [ -f $TMP/truecrypt_$2_console_amd64.tar.gz ]; then
            cd $TMP
            tar xzvf truecrypt_$2_console_amd64.tar.gz
            rm truecrypt_$2_console_amd64.tar.gz 
            test -d usr || exit 1
            sudo cp -Rv usr /
            rm -rf usr
        fi
        file /usr/bin/truecrypt
        set_truecrypt_permission
        ;;
      win)
        wget "http://id774.net/truecrypt/TrueCrypt Setup $2.exe"
        sudo mv * /usr/local/src/crypt/truecrypt
        sudo chown -R $OWNER /usr/local/src/crypt/truecrypt
        ;;
      mac)
        wget "http://id774.net/truecrypt/TrueCrypt $2 Mac OS X.dmg"
        sudo mv * /usr/local/src/crypt/truecrypt
        sudo chown -R $OWNER /usr/local/src/crypt/truecrypt
        ;;
      src)
        wget "http://id774.net/truecrypt/TrueCrypt $2 Source.tar.gz"
        sudo mv * /usr/local/src/crypt/truecrypt
        sudo chown -R $OWNER /usr/local/src/crypt/truecrypt
        ;;
    esac
    cd ..
    rm -rf install_truecrypt
}

set_truecrypt_permission() {
    sudo chown -R $OWNER /usr/local/src/crypt/truecrypt
    sudo chown -R $OWNER /usr/share/truecrypt
    sudo chown $OWNER /usr/local/src/crypt
    sudo chown $OWNER /usr/local/src
    sudo chown $OWNER /usr/bin/truecrypt
    sudo chown $OWNER /usr/bin/truecrypt-uninstall.sh
}

setup_environment() {
    which dmsetup > /dev/null || sudo aptitude -y install dmsetup
    test -d /usr/local/src/crypt/truecrypt || sudo mkdir -p /usr/local/src/crypt/truecrypt
    test -d $HOME/.tmp || mkdir $HOME/.tmp
    case $OSTYPE in
      *darwin*)
        OWNER=root:wheel
        ;;
      *)
        OWNER=root:root
        ;;
    esac
}

purge_old_version() {
    if [ `aptitude search truecrypt | awk '/^i/' | wc -l` != 0 ]; then
        sudo aptitude purge truecrypt
    fi
}

install_crypt_main() {
    test -n "$2" && TRUECRYPT_CURRENT_VERSION=$2
    test -n "$2" || TRUECRYPT_CURRENT_VERSION=7.0a
    setup_environment
    export TMP=$HOME/.tmp
    install_truecrypt $1 $TRUECRYPT_CURRENT_VERSION
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_crypt_main $*
