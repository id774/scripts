#!/bin/sh

test -d ~/crypt/arc || exit 1
test -f ~/crypt/arc/firefox.tar.gz && rm ~/crypt/arc/firefox.tar.gz
cd

case $OSTYPE in
  *darwin*)
    exit 1
    ;;
  *)
    cd ~/crypt
    tar czvf ~/crypt/arc/firefox.tar.gz .mozilla
    ;;
esac

