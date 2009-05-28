#!/bin/sh

test -f ~/arc/skype.tar.gz || exit 1
cd

case $OSTYPE in
  *darwin*)
    rm -rf  "~/Library/Application Support/Skype"
    ;;
  *)
    rm -rf ~/.Skype
    ;;
esac

tar xzvf ~/arc/skype.tar.gz

