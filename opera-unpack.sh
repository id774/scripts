#!/bin/sh

test -f ~/arc/opera.tar.gz || exit 1
cd

case $OSTYPE in
  *darwin*)
    rm -rf  "~/Library/Preferences/Opera Preferences"
    ;;
  *)
    rm -rf ~/.opera
    ;;
esac

tar xzvf ~/arc/opera.tar.gz

