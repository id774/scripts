#!/bin/sh

test -f $HOME/arc/chromium.tar.gz || exit 1
cd
rm -rf $HOME/.chromium

tar xzvf $HOME/arc/chromium.tar.gz

