#!/bin/bash

case $OSTYPE in
  *darwin*)
    OPTIONS=-Rv
    ;;
  *)
    OPTIONS=-Rvd
    ;;
esac

test -d ~/.emacs.d && rm -rf ~/.emacs.d/
test -f ~/.emacs && rm -f ~/.emacs

test -d $SCRIPTS/dot_files/dot_emacs.d || exit 1
cp $OPTIONS $SCRIPTS/dot_files/dot_emacs $HOME/.emacs
TARGET=$HOME/.emacs.d
test -d $TARGET || mkdir -p $TARGET
cp $OPTIONS $SCRIPTS/dot_files/dot_emacs.d/* $TARGET/

test -f ~/private/scripts/etc/twitter1-account.el && cp $OPTIONS ~/private/scripts/etc/twitter*-account.el $TARGET/elisp/

chmod 600 $TARGET/elisp/twitter*-account.el

