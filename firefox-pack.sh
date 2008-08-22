#!/bin/sh

test -d ~/arc || exit 1
test -f ~/arc/firefox.tar.gz && rm ~/arc/firefox.tar.gz
cd

case $OSTYPE in
  *darwin*)
    tar czvf ~/arc/firefox.tar.gz "Library/Application Support/Firefox"
    ;;
  *)
    tar czvf ~/arc/firefox.tar.gz .mozilla
    ;;
esac

