#!/bin/sh

test -f ~/crypt/arc/thunderbird.tar.gz || exit 1
cd

case $OSTYPE in
  *darwin*)
    exit 1
    ;;
  *)
    test -d .thunderbird && rm -rf ~/crypt/.thunderbird
    test -d .mozilla-thunderbird && rm -rf ~/crypt/.mozilla-thunderbird
    ;;
esac

tar xzvf ~/crypt/arc/thunderbird.tar.gz -C ~/crypt

