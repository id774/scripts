#!/bin/sh

test -f ~/arc/netbeans-config.tar.gz || exit 1
cd
rm -rf ~/.netbeans

tar xzvf ~/arc/netbeans-config.tar.gz

