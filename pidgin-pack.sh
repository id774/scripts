#!/bin/sh

test -d $HOME/arc || exit 1
test -f $HOME/arc/pidgin.tar.gz && rm $HOME/arc/pidgin.tar.gz
cd
tar czvf $HOME/arc/pidgin.tar.gz .purple
chmod 600 $HOME/arc/pidgin.tar.gz

