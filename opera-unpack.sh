#!/bin/sh

test -f ~/arc/opera.tar.gz || exit 1
cd
rm -rf ~/.opera

case $OSTYPE in
  *darwin*)
    rm -rf  "~/Library/Preferences/Opera Preferences 10"
    ;;
  *)
    rm -rf ~/.opera
    ;;
esac

tar xzvf ~/arc/opera.tar.gz

