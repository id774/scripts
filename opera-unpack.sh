#!/bin/sh

test -f $HOME/arc/opera.tar.gz || exit 1
cd

case $OSTYPE in
  *darwin*)
    rm -rf  "$HOME/Library/Preferences/Opera Preferences"
    ;;
  *)
    rm -rf $HOME/.opera
    ;;
esac

tar xzvf $HOME/arc/opera.tar.gz

