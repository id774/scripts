#!/bin/sh

test -f ~/arc/thunderbird.tar.gz || exit 1
cd

case $OSTYPE in
  *darwin*)
    rm -rf  "~/Library/Thunderbird"
    ;;
  *)
    test -d .thunderbird && rm -rf ~/.thunderbird
    test -d .mozilla-thunderbird && rm -rf ~/.mozilla-thunderbird
    ;;
esac

tar xzvf ~/arc/thunderbird.tar.gz

