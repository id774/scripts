#!/bin/sh

test -d $HOME/arc || exit 1
test -f $HOME/arc/chromium.tar.gz && rm $HOME/arc/chromium.tar.gz
cd

tar czvf $HOME/arc/chromium.tar.gz .config/chromium
chmod 600 $HOME/arc/chromium.tar.gz

