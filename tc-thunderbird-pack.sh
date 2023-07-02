#!/bin/sh

test -d $HOME/crypt/arc || exit 1
test -f $HOME/crypt/arc/thunderbird.tar.gz && rm $HOME/crypt/arc/thunderbird.tar.gz
cd

case $OSTYPE in
  *darwin*)
    exit 1
    ;;
  *)
    cd $HOME/crypt
    test -d .thunderbird && tar czvf $HOME/crypt/arc/thunderbird.tar.gz .thunderbird
    test -d .mozilla-thunderbird && tar czvf $HOME/crypt/arc/thunderbird.tar.gz .mozilla-thunderbird
    ;;
esac

chmod 600 $HOME/arc/thunderbird.tar.gz

