#!/bin/sh

test -d ~/crypt/arc || exit 1
test -f ~/crypt/arc/thunderbird.tar.gz && rm ~/crypt/arc/thunderbird.tar.gz
cd

case $OSTYPE in
  *darwin*)
    exit 1
    ;;
  *)
    cd ~/crypt
    test -d .thunderbird && tar czvf ~/crypt/arc/thunderbird.tar.gz .thunderbird
    test -d .mozilla-thunderbird && tar czvf ~/crypt/arc/thunderbird.tar.gz .mozilla-thunderbird
    ;;
esac

chmod 600 ~/arc/thunderbird.tar.gz
 
