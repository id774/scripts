#!/bin/sh

test -f $HOME/arc/pidgin.tar.gz || exit 1
cd
rm -rf $HOME/.purple
tar xzvf $HOME/arc/pidgin.tar.gz

