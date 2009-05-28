#!/bin/sh

test -d ~/arc || exit 1
test -f ~/arc/skype.tar.gz && rm ~/arc/skype.tar.gz
cd

case $OSTYPE in
  *darwin*)
    tar czvf ~/arc/skype.tar.gz "Library/Application Support/Skype"
    ;;
  *)
    tar czvf ~/arc/skype.tar.gz .mozilla
    ;;
esac

