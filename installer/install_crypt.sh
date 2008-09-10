#!/bin/sh
#
########################################################################
# Install Crypt (truecrypt and des)
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

sudo aptitude -y install dmsetup

mkdir install_crypt
cd install_crypt

# TrueCrypt
case "$1" in
  ubuntu-feisty)
    wget http://page.freett.com/railsinstall2/truecrypt-4.3a-ubuntu-7.04-x86.tar.gz
    tar xzvf truecrypt-4.3a-ubuntu-7.04-x86.tar.gz
    rm truecrypt-4.3a-ubuntu-7.04-x86.tar.gz
    cd truecrypt-4.3a
    sudo dpkg -i truecrypt_4.3a-0_i386.deb
    cd ..
    rm -rf truecrypt-4.3a
    ;;
  ubuntu-x86)
    wget http://big.freett.com/railsinstall2/truecrypt-5.1a-ubuntu-x86.tar.gz
    tar xzvf truecrypt-5.1a-ubuntu-x86.tar.gz
    rm truecrypt-5.1a-ubuntu-x86.tar.gz
    cd truecrypt-5.1a
    sudo dpkg -i truecrypt_5.1a-0_i386.deb
    wget http://big.freett.com/railsinstall2/truecrypt-5.1a-source-code.tar.gz
    sudo mkdir -p /usr/local/src/crypt/truecrypt
    sudo tar xzvf truecrypt-5.1a-source-code.tar.gz -C /usr/local/src/crypt/truecrypt
    sudo mv truecrypt-5.1a-source-code.tar.gz /usr/local/src/crypt/truecrypt
    sudo mv truecrypt_5.1a-0_i386.deb /usr/local/src/crypt/truecrypt
    sudo chmod 644 /usr/local/src/crypt/truecrypt/truecrypt-5.1a-source-code.tar.gz
    cd ..
    rm -rf truecrypt-5.1a
    ;;
  ubuntu-x64)
    wget http://big.freett.com/railsinstall2/truecrypt-5.1a-ubuntu-x64.tar.gz
    tar xzvf truecrypt-5.1a-ubuntu-x64.tar.gz
    rm truecrypt-5.1a-ubuntu-x64.tar.gz
    cd truecrypt-5.1a
    sudo dpkg -i truecrypt_5.1a-0_amd64.deb
    wget http://big.freett.com/railsinstall2/truecrypt-5.1a-source-code.tar.gz
    sudo mkdir -p /usr/local/src/crypt/truecrypt
    sudo tar xzvf truecrypt-5.1a-source-code.tar.gz -C /usr/local/src/crypt/truecrypt
    sudo mv truecrypt-5.1a-source-code.tar.gz /usr/local/src/crypt/truecrypt
    sudo mv truecrypt_5.1a-0_amd64.deb /usr/local/src/crypt/truecrypt
    sudo chmod 644 /usr/local/src/crypt/truecrypt/truecrypt-5.1a-source-code.tar.gz
    cd ..
    rm -rf truecrypt-5.1a
    ;;
  *)
    wget http://big.freett.com/railsinstall2/truecrypt-4.3a-source-code.tar.gz
    sudo mkdir -p /usr/local/src/crypt/truecrypt
    sudo tar xzvf truecrypt-4.3a-source-code.tar.gz -C /usr/local/src/crypt/truecrypt
    sudo mv truecrypt-4.3a-source-code.tar.gz /usr/local/src/crypt/truecrypt
    sudo chmod 644 /usr/local/src/crypt/truecrypt/truecrypt-4.3a-source-code.tar.gz
    ;;
esac

# DES
wget http://page.freett.com/railsinstall2/kmdes-ubuntu.tar.gz
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
rm -rf install_crypt

case $OSTYPE in
  *darwin*)
    sudo chown -R root:wheel /usr/local/src/crypt
    ;;
  *)
    sudo chown -R root:root /usr/local/src/crypt
    ;;
esac
