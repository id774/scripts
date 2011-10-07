#!/bin/sh

test -d $HOME/arc || exit 1
test -f $HOME/arc/thunderbird.tar.gz && rm $HOME/arc/thunderbird.tar.gz
cd

case $OSTYPE in
  *darwin*)
    tar czvf $HOME/arc/thunderbird.tar.gz "Library/Thunderbird"
    ;;
  *)
    test -d .thunderbird && tar czvf $HOME/arc/thunderbird.tar.gz .thunderbird
    test -d .mozilla-thunderbird && tar czvf $HOME/arc/thunderbird.tar.gz .mozilla-thunderbird
    ;;
esac

chmod 600 $HOME/arc/thunderbird.tar.gz

