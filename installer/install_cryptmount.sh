#!/bin/sh
#
########################################################################
# Install cryptmount
# See also http://cryptmount.sourceforge.net/
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 2010-02-23
#       Refactoring.
#  v1.0 2008-08-15
#       Stable.
########################################################################

test -n "$1" && CRYPT_VOLUME_SIZE=$1
test -n "$1" || CRYPT_VOLUME_SIZE=10

test -n "$2" && CRYPT_VOLUME_NAME=$2
test -n "$2" || CRYPT_VOLUME_NAME=home-ubuntu-crypt

sudo apt-get -y install cryptmount

sudo cp $SCRIPTS/etc/cmtab /etc/cryptmount/cmtab
sudo vi /etc/cryptmount/cmtab

mkdir $HOME/local
dd if=/dev/zero of=$HOME/local/crypt.fs bs=1M count=$CRYPT_VOLUME_SIZE
mkdir $HOME/crypt

sudo cryptmount --generate-key 32 $CRYPT_VOLUME_NAME
sudo cryptmount --prepare $CRYPT_VOLUME_NAME
sudo mkfs.ext3 /dev/mapper/$CRYPT_VOLUME_NAME
sudo cryptmount --release $CRYPT_VOLUME_NAME
sudo cryptmount $CRYPT_VOLUME_NAME
sudo chown $USER:root $HOME/crypt
sudo chmod 700 $HOME/crypt
sudo cryptmount -u $CRYPT_VOLUME_NAME
sudo chmod 700 $HOME/local/crypt.fs

cryptmount $CRYPT_VOLUME_NAME
