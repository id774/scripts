#!/bin/sh

test -f $HOME/arc/skype.tar.gz || exit 1
cd

case $OSTYPE in
  *darwin*)
    rm -rf  "$HOME/Library/Application Support/Skype"
    ;;
  *)
    rm -rf $HOME/.Skype
    ;;
esac

tar xzvf $HOME/arc/skype.tar.gz

