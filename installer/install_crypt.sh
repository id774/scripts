#!/bin/sh
#
########################################################################
# Install TrueCrypt
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 5/21,2009
#       Update to 6.2, add new method.
#  v1.1 1/18,2009
#       Update to 6.1a, and refactoring.
#  v1.0 8/15,2008
#       Stable.
########################################################################

set_truecrypt_permission() {
    case $OSTYPE in
      *darwin*)
        sudo chown -R root:wheel /usr/local/src/crypt/truecrypt
        sudo chown root:wheel /usr/local/src/crypt
        sudo chown root:wheel /usr/local/src
        ;;
      *)
        sudo chown -R root:root /usr/local/src/crypt/truecrypt
        sudo chown root:root /usr/local/src/crypt
        sudo chown root:root /usr/local/src
        ;;
    esac
}

get_truecrypt_source() {
    wget http://big.freett.com/railsinstall2/$1
    sudo tar xzvf $1 -C /usr/local/src/crypt/truecrypt
    sudo mv $1 /usr/local/src/crypt/truecrypt
    sudo chmod 644 /usr/local/src/crypt/truecrypt/$1
    md5.sh /usr/local/src/crypt/truecrypt/$1
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

install_truecrypt_43a() {
    wget http://page.freett.com/railsinstall2/$1
    md5.sh $1
    tar xzvf $1
    rm $1
    cd truecrypt-4.3a
    setup_truecrypt_deb $2
    cd ..
    rm -rf truecrypt-4.3a
}

install_truecrypt_51a() {
    wget http://big.freett.com/railsinstall2/$1
    md5.sh $1
    tar xzvf $1
    rm $1
    cd truecrypt-5.1a
    setup_truecrypt_deb $2
    cd ..
    rm -rf truecrypt-5.1a
}

install_truecrypt_61a() {
    wget http://big.freett.com/railsinstall2/$1
    md5.sh $1
    setup_truecrypt_deb $1
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

install_truecrypt_source() {
    wget "http://id774.net/truecrypt/$1"
    sudo tar xzvf $1 -C /usr/local/src/crypt/truecrypt
    sudo mv $1 /usr/local/src/crypt/truecrypt
    sudo chmod 644 /usr/local/src/crypt/truecrypt/$1
    md5.sh /usr/local/src/crypt/truecrypt/$1
}

install_truecrypt() {
    mkdir install_truecrypt
    cd install_truecrypt
    case "$1" in
      4.3a-ubuntu-x64)
        install_truecrypt_43a truecrypt-4.3a-ubuntu-7.04-x64.tar.gz truecrypt_4.3a-0_amd64.deb
        ;;
      4.3a-ubuntu-x86)
        install_truecrypt_43a truecrypt-4.3a-ubuntu-7.04-x86.tar.gz truecrypt_4.3a-0_i386.deb
        ;;
      4.3a-source)
        get_truecrypt_source truecrypt-4.3a-source-code.tar.gz
        ;;
      5.1a-ubuntu-x64)
        install_truecrypt_51a truecrypt-5.1a-ubuntu-x64.tar.gz truecrypt_5.1a-0_amd64.deb
        ;;
      5.1a-ubuntu-x86)
        install_truecrypt_51a truecrypt-5.1a-ubuntu-x86.tar.gz truecrypt_5.1a-0_i386.deb
        ;;
      5.1a-source)
        get_truecrypt_source truecrypt-5.1a-source-code.tar.gz
        ;;
      6.1a-ubuntu-x64)
        install_truecrypt_61a truecrypt_6.1a-0_amd64.deb
        ;;
      6.1a-ubuntu-x86)
        install_truecrypt_61a truecrypt_6.1a-0_i386.deb
        ;;
      6.1a-source)
        get_truecrypt_source TrueCrypt_6.1a_Source.tar.gz
        ;;
      6.2_i386_deb)
        install_truecrypt_deb truecrypt_6.2-0_i386.deb
        ;;
      6.2_amd64_deb)
        install_truecrypt_deb truecrypt_6.2-0_amd64.deb
        ;;
      6.2_i586_rpm)
        install_truecrypt_rpm truecrypt-6.2-0.i586.rpm
        ;;
      6.2_x86_64_rpm)
        install_truecrypt_rpm truecrypt-6.2-0.x86_64.rpm
        ;;
      6.2_win)
        sudo wget "http://id774.net/truecrypt/TrueCrypt Setup 6.2.exe"
        sudo mv * /usr/local/src/crypt/truecrypt
        ;;
      6.2_macosx)
        sudo wget "http://id774.net/truecrypt/TrueCrypt 6.2 Mac OS X.dmg"
        sudo mv * /usr/local/src/crypt/truecrypt
        ;;
      6.2_source)
        sudo wget "http://id774.net/truecrypt/TrueCrypt 6.2 Source.tar.gz"
        sudo mv * /usr/local/src/crypt/truecrypt
        ;;
    esac
    cd ..
    rm -rf install_truecrypt
    set_truecrypt_permission
}

install_crypt_main() {
    which dmsetup > /dev/null || sudo aptitude -y install dmsetup
    test -d /usr/local/src/crypt/truecrypt || sudo mkdir -p /usr/local/src/crypt/truecrypt
    install_truecrypt $1
}

install_crypt_main $1
