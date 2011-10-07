#!/bin/sh

test -f $HOME/arc/thunderbird.tar.gz || exit 1
cd

case $OSTYPE in
  *darwin*)
    rm -rf  "$HOME/Library/Thunderbird"
    ;;
  *)
    test -d .thunderbird && rm -rf $HOME/.thunderbird
    test -d .mozilla-thunderbird && rm -rf $HOME/.mozilla-thunderbird
    ;;
esac

tar xzvf $HOME/arc/thunderbird.tar.gz

