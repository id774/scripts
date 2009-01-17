#!/bin/sh
#
########################################################################
# Install Crypt (truecrypt and des)
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
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

md5_universal() {
    if [ -x $SCRIPTS/md5dir.rb ]; then
        $SCRIPTS/md5dir.rb $1
    elif [ -x $SCRIPTS/md5dir.py ]; then
        $SCRIPTS/md5dir.py $1
    else
        case $OSTYPE in
          *darwin*)
            md5 $1
            ;;
          *)
            md5sum $1
            ;;
        esac
    fi
}

get_truecrypt_source() {
    wget http://big.freett.com/railsinstall2/$1
    test -d /usr/local/src/crypt/truecrypt || sudo mkdir -p /usr/local/src/crypt/truecrypt
    sudo tar xzvf $1 -C /usr/local/src/crypt/truecrypt
    sudo mv $1 /usr/local/src/crypt/truecrypt
    sudo chmod 644 /usr/local/src/crypt/truecrypt/$1
    md5_universal /usr/local/src/crypt/truecrypt/$1
}

setup_truecrypt_deb() {
    sudo dpkg -i $1
    sudo mv $1 /usr/local/src/crypt/truecrypt
    sudo chmod 644 /usr/local/src/crypt/truecrypt/$1
}

install_truecrypt_43a() {
    wget http://page.freett.com/railsinstall2/$1
    md5_universal $1
    tar xzvf $1
    rm $1
    cd truecrypt-4.3a
    setup_truecrypt_deb $2
    cd ..
    rm -rf truecrypt-4.3a
}

install_truecrypt_51a() {
    wget http://big.freett.com/railsinstall2/$1
    md5_universal $1
    tar xzvf $1
    rm $1
    cd truecrypt-5.1a
    setup_truecrypt_deb $2
    cd ..
    rm -rf truecrypt-5.1a
}

install_truecrypt_61a() {
    wget http://big.freett.com/railsinstall2/$1
    md5_universal $1
    setup_truecrypt_deb $1
}

install_truecrypt() {
    mkdir install_truecrypt
    cd install_truecrypt
    case "$1" in
      4.3a-ubuntu-x64)
        get_truecrypt_source truecrypt-4.3a-source-code.tar.gz
        install_truecrypt_43a truecrypt-4.3a-ubuntu-7.04-x64.tar.gz truecrypt_4.3a-0_amd64.deb
        ;;
      4.3a-ubuntu-x86)
        get_truecrypt_source truecrypt-4.3a-source-code.tar.gz
        install_truecrypt_43a truecrypt-4.3a-ubuntu-7.04-x86.tar.gz truecrypt_4.3a-0_i386.deb
        ;;
      4.3a-source)
        get_truecrypt_source truecrypt-4.3a-source-code.tar.gz
        ;;
      5.1a-ubuntu-x64)
        get_truecrypt_source truecrypt-5.1a-source-code.tar.gz
        install_truecrypt_51a truecrypt-5.1a-ubuntu-x64.tar.gz truecrypt_5.1a-0_amd64.deb
        ;;
      5.1a-ubuntu-x86)
        get_truecrypt_source truecrypt-5.1a-source-code.tar.gz
        install_truecrypt_51a truecrypt-5.1a-ubuntu-x86.tar.gz truecrypt_5.1a-0_i386.deb
        ;;
      5.1a-source)
        get_truecrypt_source truecrypt-5.1a-source-code.tar.gz
        ;;
      6.1a-ubuntu-x64)
        get_truecrypt_source TrueCrypt_6.1a_Source.tar.gz
        install_truecrypt_61a truecrypt_6.1a-0_amd64.deb
        ;;
      6.1a-ubuntu-x86)
        get_truecrypt_source TrueCrypt_6.1a_Source.tar.gz
        install_truecrypt_61a truecrypt_6.1a-0_i386.deb
        ;;
      *)
        get_truecrypt_source TrueCrypt_6.1a_Source.tar.gz
        ;;
    esac
    cd ..
    rm -rf install_truecrypt
    set_truecrypt_permission
}

set_des_permission() {
    case $OSTYPE in
      *darwin*)
        sudo chown -R root:wheel /usr/local/src/crypt/des
        ;;
      *)
        sudo chown -R root:root /usr/local/src/crypt/des
        ;;
    esac
}

install_des() {
    mkdir install_des
    cd install_des
    wget http://page.freett.com/railsinstall2/kmdes-ubuntu.tar.gz
    md5_universal kmdes-ubuntu.tar.gz
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
    set_des_permission
}

install_crypt_main() {
    which dmsetup || sudo aptitude -y install dmsetup
    install_truecrypt $1
    which des || install_des
}

install_crypt_main $1
