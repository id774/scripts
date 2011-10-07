#!/bin/sh

test -f $HOME/arc/netbeans-config.tar.gz || exit 1
cd
rm -rf $HOME/.netbeans

tar xzvf $HOME/arc/netbeans-config.tar.gz

