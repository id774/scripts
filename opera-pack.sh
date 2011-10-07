#!/bin/sh

test -d $HOME/arc || exit 1
test -f $HOME/arc/opera.tar.gz && rm $HOME/arc/opera.tar.gz
cd

case $OSTYPE in
  *darwin*)
    tar czvf $HOME/arc/opera.tar.gz "Library/Preferences/Opera Preferences"
    ;;
  *)
    tar czvf $HOME/arc/opera.tar.gz .opera
    ;;
esac

chmod 600 $HOME/arc/opera.tar.gz

