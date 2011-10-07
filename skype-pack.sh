#!/bin/sh

test -d $HOME/arc || exit 1
test -f $HOME/arc/skype.tar.gz && rm $HOME/arc/skype.tar.gz
cd

case $OSTYPE in
  *darwin*)
    tar czvf $HOME/arc/skype.tar.gz "Library/Application Support/Skype"
    ;;
  *)
    tar czvf $HOME/arc/skype.tar.gz .Skype
    ;;
esac

chmod 600 $HOME/arc/skype.tar.gz

