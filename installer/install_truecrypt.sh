#!/bin/sh
#
########################################################################
# Install TrueCrypt 7
#  $1 = architecture
#  $2 = version
#  $3 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.4 2/8,2012
#       Update to 7.1a.
#  v0.3 9/9,2011
#       Update to 7.1.
#  v0.2 9/16,2010
#       Refactoring.
#  v0.1 8/7,2010
#       Stable.
########################################################################

setup_environment() {
    which dmsetup > /dev/null || sudo apt-get -y install dmsetup
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

set_truecrypt_permission() {
    sudo chown -R $OWNER /usr/local/src/crypt/truecrypt
    sudo chown -R $OWNER /usr/share/truecrypt
    sudo chown $OWNER /usr/local/src/crypt
    sudo chown $OWNER /usr/local/src
    sudo chown $OWNER /usr/bin/truecrypt
    sudo chown $OWNER /usr/bin/truecrypt-uninstall.sh
}

save_packages() {
    sudo cp $1 $2
    sudo chown $OWNER $2/$1
}

save_sources() {
    sudo mv * /usr/local/src/crypt/truecrypt
    sudo chown -R $OWNER /usr/local/src/crypt/truecrypt
}

install_truecrypt() {
    mkdir install_truecrypt
    cd install_truecrypt
    case "$1" in
      linux-i386 | linux-i686)
        wget "http://id774.net/truecrypt/truecrypt-$2-linux-console-x86.tar.gz"
        test -n "$3" || save_packages truecrypt-$2-linux-console-x86.tar.gz /usr/local/src/crypt/truecrypt
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
      linux-amd64 | linux-x86_64)
        wget "http://id774.net/truecrypt/truecrypt-$2-linux-console-x64.tar.gz"
        test -n "$3" || save_packages truecrypt-$2-linux-console-x64.tar.gz /usr/local/src/crypt/truecrypt
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
        save_sources
        ;;
      mac)
        wget "http://id774.net/truecrypt/TrueCrypt $2 Mac OS X.dmg"
        save_sources
        ;;
      src)
        wget "http://id774.net/truecrypt/TrueCrypt $2 Source.tar.gz"
        save_sources
        ;;
    esac
    cd ..
    rm -rf install_truecrypt
}

install_crypt_main() {
    test -n "$2" && TRUECRYPT_CURRENT_VERSION=$2
    test -n "$2" || TRUECRYPT_CURRENT_VERSION=7.1a
    setup_environment $*
    export TMP=$HOME/.tmp
    install_truecrypt $1 $TRUECRYPT_CURRENT_VERSION $3
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_crypt_main $*
