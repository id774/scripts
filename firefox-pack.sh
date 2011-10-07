#!/bin/sh

test -d $HOME/arc || exit 1
test -f $HOME/arc/firefox.tar.gz && rm $HOME/arc/firefox.tar.gz
cd

case $OSTYPE in
  *darwin*)
    tar czvf $HOME/arc/firefox.tar.gz "Library/Application Support/Firefox"
    ;;
  *)
    tar czvf $HOME/arc/firefox.tar.gz .mozilla
    ;;
esac

chmod 600 $HOME/arc/firefox.tar.gz

