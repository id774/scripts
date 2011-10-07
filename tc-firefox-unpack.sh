#!/bin/sh

test -f $HOME/crypt/arc/firefox.tar.gz || exit 1
cd

case $OSTYPE in
  *darwin*)
    exit 1
    ;;
  *)
    rm -rf $HOME/crypt/.mozilla
    ;;
esac

tar xzvf $HOME/crypt/arc/firefox.tar.gz -C $HOME/crypt

