#!/bin/sh

test -f $HOME/crypt/arc/thunderbird.tar.gz || exit 1
cd

case $OSTYPE in
  *darwin*)
    exit 1
    ;;
  *)
    test -d .thunderbird && rm -rf $HOME/crypt/.thunderbird
    test -d .mozilla-thunderbird && rm -rf $HOME/crypt/.mozilla-thunderbird
    ;;
esac

tar xzvf $HOME/crypt/arc/thunderbird.tar.gz -C $HOME/crypt

