#!/bin/sh

test -f ~/crypt/arc/firefox.tar.gz || exit 1
cd

case $OSTYPE in
  *darwin*)
    exit 1
    ;;
  *)
    rm -rf ~/crypt/.mozilla
    ;;
esac

tar xzvf ~/crypt/arc/firefox.tar.gz -C ~/crypt

