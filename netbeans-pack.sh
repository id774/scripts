#!/bin/sh

test -d $HOME/arc || exit 1
test -f $HOME/arc/netbeans-config.tar.gz && rm $HOME/arc/netbeans-config.tar.gz
cd
tar czvf $HOME/arc/netbeans-config.tar.gz .netbeans
chmod 600 $HOME/arc/netbeans-config.tar.gz

