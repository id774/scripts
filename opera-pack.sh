#!/bin/sh

test -d ~/arc || exit 1
test -f ~/arc/opera.tar.gz && rm ~/arc/opera.tar.gz
cd

case $OSTYPE in
  *darwin*)
    tar czvf ~/arc/opera.tar.gz "Library/Preferences/Opera Preferences"
    ;;
  *)
    tar czvf ~/arc/opera.tar.gz .opera
    ;;
esac

chmod 600 ~/arc/opera.tar.gz

