#!/bin/sh

test -d ~/arc || exit 1
test -f ~/arc/netbeans-config.tar.gz && rm ~/arc/netbeans-config.tar.gz
cd
tar czvf ~/arc/netbeans-config.tar.gz .netbeans

