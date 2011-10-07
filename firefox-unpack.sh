#!/bin/sh

test -f $HOME/arc/firefox.tar.gz || exit 1
cd

case $OSTYPE in
  *darwin*)
    rm -rf  "$HOME/Library/Application Support/Firefox"
    ;;
  *)
    rm -rf $HOME/.mozilla
    ;;
esac

tar xzvf $HOME/arc/firefox.tar.gz

