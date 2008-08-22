#!/bin/sh

test -f ~/arc/firefox.tar.gz || exit 1
cd

case $OSTYPE in
  *darwin*)
    rm -rf  "~/Library/Application Support/Firefox"
    ;;
  *)
    rm -rf ~/.mozilla
    ;;
esac

tar xzvf ~/arc/firefox.tar.gz

