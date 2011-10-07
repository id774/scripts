#!/bin/sh

test -d $HOME/crypt/arc || exit 1
test -f $HOME/crypt/arc/firefox.tar.gz && rm $HOME/crypt/arc/firefox.tar.gz
cd

case $OSTYPE in
  *darwin*)
    exit 1
    ;;
  *)
    cd $HOME/crypt
    tar czvf $HOME/crypt/arc/firefox.tar.gz .mozilla
    ;;
esac

chmod 600 $HOME/arc/firefox.tar.gz

