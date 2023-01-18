#!/bin/sh
#
########################################################################
# Install VeraCrypt
#  $1 = architecture
#  $2 = version
#  $3 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 1/18,2023
#       Forked from veracrypt.
########################################################################

setup_environment() {
    which dmsetup > /dev/null || sudo apt-get -y install dmsetup
    test -d /usr/local/src/crypt/veracrypt || sudo mkdir -p /usr/local/src/crypt/veracrypt
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

set_veracrypt_permission() {
    sudo chown -R $OWNER /usr/local/src/crypt/veracrypt
    sudo chown -R $OWNER /usr/share/veracrypt
    sudo chown $OWNER /usr/local/src/crypt
    sudo chown $OWNER /usr/local/src
    sudo chown $OWNER /usr/bin/veracrypt
    sudo chown $OWNER /usr/bin/veracrypt-uninstall.sh
}

save_packages() {
    sudo cp $1 $2
    sudo chown $OWNER $2/$1
}

save_sources() {
    sudo mv * /usr/local/src/crypt/veracrypt
    sudo chown -R $OWNER /usr/local/src/crypt/veracrypt
}

install_veracrypt() {
    mkdir install_veracrypt
    cd install_veracrypt
    case "$1" in
      linux-i386 | linux-i686)
        wget "http://id774.net/veracrypt/veracrypt-$2-setup-console-x86"
        test -n "$3" || save_packages veracrypt-$2-setup-console-x86 /usr/local/src/crypt/veracrypt
        chmod +x ./veracrypt-$2-setup-console-x86 && ./veracrypt-$2-setup-console-x86
        if [ -f $TMP/veracrypt_$2_console_i386.tar.gz ]; then
            cd $TMP
            tar xzvf veracrypt_$2_console_i386.tar.gz
            rm veracrypt_$2_console_i386.tar.gz
            test -d usr || exit 1
            sudo cp -Rv usr /
            rm -rf usr
        fi
        file /usr/bin/veracrypt
        set_veracrypt_permission
        ;;
      linux-amd64 | linux-x86_64)
        wget "http://id774.net/veracrypt/veracrypt-$2-setup-console-x64"
        test -n "$3" || save_packages veracrypt-$2-setup-console-x64 /usr/local/src/crypt/veracrypt
        chmod +x ./veracrypt-$2-setup-console-x64 && ./veracrypt-$2-setup-console-x64
        if [ -f $TMP/veracrypt_$2_console_amd64.tar.gz ]; then
            cd $TMP
            tar xzvf veracrypt_$2_console_amd64.tar.gz
            rm veracrypt_$2_console_amd64.tar.gz
            test -d usr || exit 1
            sudo cp -Rv usr /
            rm -rf usr
        fi
        file /usr/bin/veracrypt
        set_veracrypt_permission
        ;;
      win)
        wget "http://id774.net/veracrypt/VeraCrypt Setup $2.exe"
        save_sources
        ;;
      mac)
        wget "http://id774.net/veracrypt/VeraCrypt_$2.dmg"
        save_sources
        ;;
      src)
        wget "http://id774.net/veracrypt/VeraCrypt_$2_Source.tar.bz2"
        save_sources
        ;;
    esac
    cd ..
    rm -rf install_veracrypt
}

install_crypt_main() {
    test -n "$2" && VERACRYPT_CURRENT_VERSION=$2
    test -n "$2" || VERACRYPT_CURRENT_VERSION=1.25.9
    setup_environment $*
    export TMP=$HOME/.tmp
    install_veracrypt $1 $VERACRYPT_CURRENT_VERSION $3
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_crypt_main $*
