#!/bin/sh

test -d ~/arc || exit 1
test -f ~/arc/thunderbird.tar.gz && rm ~/arc/thunderbird.tar.gz
cd

case $OSTYPE in
  *darwin*)
    tar czvf ~/arc/thunderbird.tar.gz "Library/Thunderbird"
    ;;
  *)
    test -d .thunderbird && tar czvf ~/arc/thunderbird.tar.gz .thunderbird
    test -d .mozilla-thunderbird && tar czvf ~/arc/thunderbird.tar.gz .mozilla-thunderbird
    ;;
esac

