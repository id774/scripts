#!/bin/bash

case $OSTYPE in
  *darwin*)
    OPTIONS=-Rv
    ;;
  *)
    OPTIONS=-Rvd
    ;;
esac

test -d $SCRIPTS/dot_files/dot_vim || exit 1
TARGET=$HOME/.vim
test -d $TARGET || mkdir -p $TARGET
cp $OPTIONS $SCRIPTS/dot_files/dot_vim/* $TARGET/

